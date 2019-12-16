open System
open System.IO

let rec processInputs (startX, startY) (inputs: string list) =
    match inputs with
    | input :: rest ->
        let moveCount = Seq.tail input |> Array.ofSeq |> String |> Int32.Parse
        let coordinates =
            match Seq.head input with
            | 'R' -> seq { for x in startX + 1 .. startX + moveCount do yield (x, startY) }
            | 'U' -> seq { for y in startY + 1 .. startY + moveCount do yield (startX, y) }
            | 'L' -> seq { for x in startX - 1 .. -1 .. startX - moveCount do yield (x, startY) }
            | 'D' -> seq { for y in startY - 1 .. -1 .. startY - moveCount do yield (startX, y) }
            | _ -> failwith "Invalid direction"

        Seq.append (processInputs (Seq.last coordinates) rest) coordinates
    | [] -> Seq.empty

let manhattanDistance (x, y) = abs x + abs y

let part1 intersections =
    Seq.map manhattanDistance intersections
    |> Seq.min


let part2 firstWire secondWire intersections =
    Set.map (fun position -> (Seq.findIndex ((=) position) firstWire + Seq.findIndex ((=) position) secondWire) + 2) intersections
    |> Set.minElement

[<EntryPoint>]
let main argv =
    let wires =
        File.ReadAllLines("input.txt")
        |> Array.map ((fun line -> line.Split ',') >> List.ofArray >> processInputs (0, 0))

    let firstWire = Array.head wires
    let secondWire = Array.last wires
    let intersections = Set.intersect (Set.ofSeq firstWire) (Set.ofSeq secondWire)
    printfn "%d" (part1 intersections)
    printfn "%d" (part2 firstWire secondWire intersections)
    0 // return an integer exit code
