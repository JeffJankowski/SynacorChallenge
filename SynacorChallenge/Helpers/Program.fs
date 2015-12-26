// F# for Scientists (page 166-167)
//*********************************
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)
//*********************************

let coinCombo = 
    let nums = [2;3;5;7;9]
    let perms = permute nums
    perms
    |> List.filter (fun l -> 
        let arr = List.toArray l
        arr.[0] + arr.[1] * (pown arr.[2] 2) + (pown arr.[3] 3) - arr.[4] = 399 )


[<EntryPoint>]
let main argv = 
    printfn "%A" coinCombo

    System.Console.Read ()