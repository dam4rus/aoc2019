module aoc2

open System
open System.IO

let opCodeOperation = function
    | 1 -> (+)
    | 2 -> (*)
    | _ -> failwith "Invalid opcode"

let rec processOpCode offset state =
    match Array.item offset state with
    | 1 | 2 as n ->
        let pos1 = state.[offset + 1]
        let pos2 = state.[offset + 2]
        let resultPos = state.[offset + 3]
        state.[resultPos] <- (opCodeOperation n) state.[pos1] state.[pos2]
        processOpCode (offset + 4) state
    | 99 -> state
    | _ -> failwith "Invalid opcode"

let step1 input =
    let input' = Array.copy input
    input'.[1] <- 12
    input'.[2] <- 2
    processOpCode 0 input'

let step2 input =
    seq {
        for noun in 0..99 do
            for verb in 0..99 do
                yield (noun, verb)
    }
    |> Seq.find (fun (noun, verb) ->
        let input' = Array.copy input
        input'.[1] <- noun
        input'.[2] <- verb
        
        Array.head (processOpCode 0 input') = 19690720
    )

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt") |> fun input -> input.Split(",") |> Array.map Int32.Parse

    printfn "%A" <| Array.head (step1 input)
    printfn "%d%d" <|| step2 input
    0 // return an integer exit code
