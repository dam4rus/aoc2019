open System
open System.IO

let rec processInputs (startX, startY) (inputs: string list) =
    match inputs with
    | input :: rest ->
        let moveCount = Seq.tail input |> Array.ofSeq |> String |> Int32.Parse
        let coordinates =
            match Seq.head input with
            | 'R' -> [ for x in startX .. startX + moveCount do yield (x, startY) ]
            | 'U' -> [ for y in startY .. startY + moveCount do yield (startX, y) ]
            | 'L' -> [ for x in startX .. -1 .. startX - moveCount do yield (x, startY) ]
            | 'D' -> [ for y in startY .. -1 .. startY - moveCount do yield (startX, y) ]
            | _ -> failwith "Invalid direction"

        coordinates @ processInputs (List.last coordinates) rest
    | [] -> []

let manhattanDistance (x, y) = abs x + abs y

let part1 input =
    Set.intersect (Array.head input) (Array.last input)
    |> Set.remove (0, 0)
    |> Seq.map manhattanDistance
    |> Seq.min

[<EntryPoint>]
let main argv =
    let wires =
        File.ReadAllLines("input.txt")
        |> Array.map ((fun line -> line.Split ',') >> List.ofArray >> processInputs (0, 0) >> Set.ofList)

    printfn "%d" (part1 wires)
    0 // return an integer exit code
