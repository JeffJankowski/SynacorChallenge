// Jeff Jankowski 2015
// https://challenge.synacor.com/

open System
open System.Text
open System.Collections.Generic

let VALS_15B = 32768us
let (reg: uint16[]) = Array.zeroCreate 8
let stack = Stack<uint16> ()
let mutable pc = 0
let mutable op = 21us //no-op

let error msg = 
    printfn "ERROR: %s" msg
    op <- 0us

let mods x = x % VALS_15B
let modi = mods >> int
let getv x = 
    if x < VALS_15B then x 
    elif x < (VALS_15B + 8us) then reg.[modi x]
    else 
        error "invalid value"
        0us

let dump (mem: uint16[]) = 
    if not (IO.Directory.Exists "dump") then IO.Directory.CreateDirectory("dump") |> ignore
    let dpath = IO.Path.Combine(Environment.CurrentDirectory, "dump")
    let stackstr = String.Join("\n", stack.ToArray() |> Array.map (fun v -> v.ToString()))
    let regstr = String.Join("\n", reg |> Array.map (fun n -> sprintf "%d" n))
    //pc
    IO.File.WriteAllText(IO.Path.Combine(dpath, "pc"), pc.ToString())
    //stack
    IO.File.WriteAllText(IO.Path.Combine(dpath, "stack"), stackstr)
    //registers
    IO.File.WriteAllText(IO.Path.Combine(dpath, "reg"), regstr)
    //memory
    let (bytearr: byte[]) = Array.zeroCreate (mem.Length * 2)
    Buffer.BlockCopy(mem, 0, bytearr, 0, bytearr.Length)
    IO.File.WriteAllBytes(IO.Path.Combine(dpath, "mem.bin"), bytearr)


[<EntryPoint>]
let main argv = 
    let savepath = IO.Path.Combine([|"..";"..";"saves";"teleporter"|])
    let path file = IO.Path.Combine(savepath, file)
    //load program into memory
//    let bin = IO.File.ReadAllBytes "..\..\challenge.bin"
    let bin = IO.File.ReadAllBytes (path "mem.bin")
    let mem = Array.init (bin.Length / 2) (fun i -> BitConverter.ToUInt16 (bin, i * 2))

    //load save point
    pc <- IO.File.ReadAllLines(path "pc").[0] |> Int32.Parse
    IO.File.ReadAllLines(path "stack") |> List.ofArray |> List.rev |> List.iter (fun n -> stack.Push (UInt16.Parse n))
    IO.File.ReadAllLines(path"reg") |> Array.iteri (fun i e -> reg.[i] <- UInt16.Parse e)


    //fuck with 8th register
    reg.[7] <- 1us

    
    let arg n = mem.[pc+n]

    let sw = new IO.StreamWriter(path "log")
    let log (str: string) =  
        sw.WriteLine str
    
    //execute program
    while op <> 0us do
        //fetch next instruction
        op <- mem.[pc]

        match op with
        | 1us -> // set a b
            reg.[modi (arg 1)] <- getv (arg 2)
            pc <- pc + 3
        | 2us -> // push a
            stack.Push (getv (arg 1))
            pc <- pc + 2
        | 3us -> // pop a
            if stack.Count > 0 then reg.[modi (arg 1)] <- stack.Pop ()
            else error "empty stack"
            pc <- pc + 2
        | 4us -> // eq a b c
            reg.[modi (arg 1)] <- if (getv (arg 2)) = (getv (arg 3)) then 1us else 0us
            pc <- pc + 4
        | 5us -> // gt a b c
            reg.[modi (arg 1)] <- if (getv (arg 2)) > (getv (arg 3)) then 1us else 0us
            pc <- pc + 4
        | 6us -> // jmp a
            pc <- getv (arg 1) |> int
        | 7us -> // jt a b
            pc <- if getv (arg 1) <> 0us then getv (arg 2) |> int else pc + 3
        | 8us -> // jf a b
            pc <- if getv (arg 1) = 0us then getv (arg 2) |> int else pc + 3
        | 9us -> // add a b c
            reg.[modi (arg 1)] <- mods (getv (arg 2) + getv (arg 3))
            pc <- pc + 4
        | 10us -> // mult a b c
            reg.[modi (arg 1)] <- mods (getv (arg 2) * getv (arg 3))
            pc <- pc + 4
        | 11us -> // mod a b c
            reg.[modi (arg 1)] <- getv (arg 2) % getv (arg 3)
            pc <- pc + 4
        | 12us -> // and a b c
            reg.[modi (arg 1)] <- getv (arg 2) &&& getv (arg 3)
            pc <- pc + 4
        | 13us -> // or a b c
            reg.[modi (arg 1)] <- getv (arg 2) ||| getv (arg 3)
            pc <- pc + 4
        | 14us -> // not a b
            reg.[modi (arg 1)] <- ~~~(getv (arg 2)) &&& (0x7FFFus)
            pc <- pc + 3
        | 15us -> // rmem a b
            reg.[modi (arg 1)] <- mem.[getv (arg 2) |> int]
            pc <- pc + 3
        | 16us -> // wmem a b
            mem.[getv (arg 1) |> int] <- getv (arg 2)
            pc <- pc + 3
        | 17us -> // call a
            stack.Push (uint16 pc + 2us)
            pc <- getv (arg 1) |> int
        | 18us -> // ret
            if stack.Count > 0 then pc <- stack.Pop () |> int
            else op <- 0us
        | 19us -> // out a
            getv (arg 1) |> char |> printf "%c"
            pc <- pc + 2
        | 20us -> // in a
            let inp = Console.Read ()
            if inp = int '?' then dump mem elif inp = int '!' then op <- 0us

            reg.[modi (arg 1)] <- (if inp = int '\r' then uint16 '\n' else uint16 inp)
            pc <- pc + 2
        | 21us -> pc <- pc + 1
        | _ -> 
            error "invalid op"
            pc <- pc + 1

    printfn "Terminating program..."

//    sw.Flush()
//    sw.Close()

    Console.Read ()
