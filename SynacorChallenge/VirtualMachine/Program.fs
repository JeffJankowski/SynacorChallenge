// Jeff Jankowski 2015
// https://challenge.synacor.com/

open System
open System.Collections.Generic

let VALS_15B = 32768us
let (reg: uint16[]) = Array.zeroCreate 8
let stack = Stack<uint16> ()
let mutable pc = 0
let mutable op = 21us //no-op

let error msg = 
    printfn "ERROR: %s" msg
    //op <- 0us

let mods x = x % VALS_15B
let modi = mods >> int
let getv x = 
    if x < VALS_15B then x 
    elif x < (VALS_15B + 8us) then reg.[modi x]
    else 
        error "invalid value"
        0us


[<EntryPoint>]
let main argv = 
    //load program into memory
    let bin = IO.File.ReadAllBytes "..\..\challenge.bin"
    let mem = Array.init (bin.Length / 2) (fun i -> BitConverter.ToUInt16 (bin, i * 2))
    let arg n = mem.[pc+n]
    
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
            reg.[modi (arg 1)]  <- Console.Read () |> uint16
            pc <- pc + 2
        | 21us -> pc <- pc + 1
        | _ -> 
            error "invalid op"
            pc <- pc + 1


    printfn "Terminating program..."



    Console.Read ()
