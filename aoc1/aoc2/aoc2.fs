module aoc2

open System
open System.IO

let rec processOpCode offset (state: byref<int array>) =
    match Seq.skip offset state |> Seq.truncate 4 |> List.ofSeq with
    | 1::pos1::pos2::resultPos::_ ->
        state.[resultPos] <- (state.[pos1] + state.[pos2])
        processOpCode (offset + 4) &state
    | 2::pos1::pos2::resultPos::_ ->
        state.[resultPos] <- (state.[pos1] * state.[pos2])
        processOpCode (offset + 4) &state
    | 99::_ -> ()
    | _ -> failwith "Invalid opcode"

let step1 input =
    let mutable input' = Array.copy input
    input'.[1] <- 12
    input'.[2] <- 2
    processOpCode 0 &input'

let step2 input =
    seq {
        for noun in 0..99 do
            for verb in 0..99 do
                yield (noun, verb)
    }
    |> Seq.find (fun (noun, verb) ->
        let mutable input' = Array.copy input
        input'.[1] <- noun
        input'.[2] <- verb
        processOpCode 0 &input'
        Array.head input' = 19690720
    )


[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt") |> fun input -> input.Split(",") |> Array.map Int32.Parse
    step1 input

    printfn "%A" <| Array.head input
    printfn "%d%d" <|| step2 input
    0 // return an integer exit code
