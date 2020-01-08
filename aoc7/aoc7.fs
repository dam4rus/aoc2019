open Intcode

let input = [| 3; 8; 1001; 8; 10; 8; 105; 1; 0; 0; 21; 38; 59; 84; 97; 110; 191; 272; 353; 434; 99999; 3; 9; 1002; 9; 2; 9; 101; 4; 9; 9; 1002; 9; 2; 9; 4; 9; 99; 3; 9; 102; 5; 9; 9; 1001; 9; 3; 9; 1002; 9; 5; 9; 101; 5; 9; 9; 4; 9; 99; 3; 9; 102; 5; 9; 9; 101; 5; 9; 9; 1002; 9; 3; 9; 101; 2; 9; 9; 1002; 9; 4; 9; 4; 9; 99; 3; 9; 101; 3; 9; 9; 1002; 9; 3; 9; 4; 9; 99; 3; 9; 102; 5; 9; 9; 1001; 9; 3; 9; 4; 9; 99; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 99; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 1001; 9; 1; 9; 4; 9; 3; 9; 1001; 9; 1; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 99; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 1001; 9; 1; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 99; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 101; 2; 9; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 99; 3; 9; 1001; 9; 1; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 3; 9; 1002; 9; 2; 9; 4; 9; 3; 9; 1001; 9; 1; 9; 4; 9; 3; 9; 102; 2; 9; 9; 4; 9; 3; 9; 1001; 9; 2; 9; 4; 9; 3; 9; 101; 1; 9; 9; 4; 9; 99 |]

let rec insertions x = function
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
    | [] -> [[x]]

let rec permutations = function
    | x :: xs -> Seq.collect (insertions x) (permutations xs)
    | [] -> seq [ [] ]

let part1 () =
    permutations [ 0L; 1L; 2L; 3L; 4L ]
    |> Seq.map (List.fold (fun state phase -> Program.create (Some <| Draining [ phase; state ]) (Array.map int64 input) |> Program.iterateOutput |> Seq.last) 0L)
    |> Seq.max

let part2 () =
    let rec loop amplifierIndex (amplifiers: Program []) =
        let nextAmplifierIndex = (amplifierIndex + 1) % 5
        match Program.processOpCode amplifiers.[amplifierIndex] with
        | Program.Output (output, state) ->
            let newInput =
                match amplifiers.[nextAmplifierIndex].input with
                | Some (Input.Draining oldInput) -> oldInput @ [ output ]
                | _ -> [ output ]

            amplifiers.[amplifierIndex] <- state
            amplifiers.[nextAmplifierIndex] <- { amplifiers.[nextAmplifierIndex] with input = Some (Input.Draining newInput) }
            loop nextAmplifierIndex amplifiers
        | Program.ProgramEnd ->
            if amplifierIndex = 4 then
                amplifiers 
            else
                loop nextAmplifierIndex amplifiers

    permutations [ 5L; 6L; 7L; 8L; 9L ]
    |> Seq.map (
        Seq.indexed
        >> Seq.map (fun (idx, phase) ->
            let amplifierInputs = if idx = 0 then [ phase; 0L ] else [ phase ]
            Program.create (Some <| Input.Draining amplifierInputs) (Array.map int64 input)
        )
        >> Array.ofSeq
        >> loop 0
        >> Array.last
        >> fun amp -> amp.lastOutput |> Option.get
    )
    |> Seq.max

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    printfn "%d" (part2 ())
    0 // return an integer exit code
