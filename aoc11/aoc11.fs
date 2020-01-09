open System
open Intcode

let input = [| 3L; 8L; 1005L; 8L; 318L; 1106L; 0L; 11L; 0L; 0L; 0L; 104L; 1L; 104L; 0L; 3L; 8L; 1002L; 8L; -1L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 1008L; 8L; 0L; 10L; 4L; 10L; 102L; 1L; 8L; 29L; 1006L; 0L; 99L; 1006L; 0L; 81L; 1006L; 0L; 29L; 3L; 8L; 102L; -1L; 8L; 10L; 1001L; 10L; 1L; 10L; 4L; 10L; 108L; 1L; 8L; 10L; 4L; 10L; 1001L; 8L; 0L; 59L; 3L; 8L; 102L; -1L; 8L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 1008L; 8L; 1L; 10L; 4L; 10L; 102L; 1L; 8L; 82L; 1L; 1103L; 3L; 10L; 2L; 104L; 14L; 10L; 3L; 8L; 102L; -1L; 8L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 108L; 1L; 8L; 10L; 4L; 10L; 102L; 1L; 8L; 111L; 1L; 108L; 2L; 10L; 2L; 1101L; 7L; 10L; 1L; 1L; 8L; 10L; 1L; 1009L; 5L; 10L; 3L; 8L; 1002L; 8L; -1L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 108L; 0L; 8L; 10L; 4L; 10L; 102L; 1L; 8L; 149L; 3L; 8L; 1002L; 8L; -1L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 1008L; 8L; 1L; 10L; 4L; 10L; 101L; 0L; 8L; 172L; 3L; 8L; 1002L; 8L; -1L; 10L; 1001L; 10L; 1L; 10L; 4L; 10L; 108L; 0L; 8L; 10L; 4L; 10L; 1001L; 8L; 0L; 193L; 1006L; 0L; 39L; 2L; 103L; 4L; 10L; 2L; 1103L; 20L; 10L; 3L; 8L; 1002L; 8L; -1L; 10L; 1001L; 10L; 1L; 10L; 4L; 10L; 1008L; 8L; 0L; 10L; 4L; 10L; 102L; 1L; 8L; 227L; 1L; 1106L; 8L; 10L; 2L; 109L; 15L; 10L; 2L; 106L; 14L; 10L; 3L; 8L; 102L; -1L; 8L; 10L; 101L; 1L; 10L; 10L; 4L; 10L; 1008L; 8L; 1L; 10L; 4L; 10L; 101L; 0L; 8L; 261L; 3L; 8L; 102L; -1L; 8L; 10L; 1001L; 10L; 1L; 10L; 4L; 10L; 1008L; 8L; 0L; 10L; 4L; 10L; 102L; 1L; 8L; 283L; 1L; 1109L; 9L; 10L; 2L; 1109L; 5L; 10L; 2L; 1L; 2L; 10L; 1006L; 0L; 79L; 101L; 1L; 9L; 9L; 1007L; 9L; 1087L; 10L; 1005L; 10L; 15L; 99L; 109L; 640L; 104L; 0L; 104L; 1L; 21101L; 936333124392L; 0L; 1L; 21101L; 0L; 335L; 0L; 1106L; 0L; 439L; 21102L; 1L; 824663880596L; 1L; 21102L; 346L; 1L; 0L; 1105L; 1L; 439L; 3L; 10L; 104L; 0L; 104L; 1L; 3L; 10L; 104L; 0L; 104L; 0L; 3L; 10L; 104L; 0L; 104L; 1L; 3L; 10L; 104L; 0L; 104L; 1L; 3L; 10L; 104L; 0L; 104L; 0L; 3L; 10L; 104L; 0L; 104L; 1L; 21102L; 1L; 179519553539L; 1L; 21101L; 393L; 0L; 0L; 1106L; 0L; 439L; 21102L; 46266515623L; 1L; 1L; 21101L; 0L; 404L; 0L; 1106L; 0L; 439L; 3L; 10L; 104L; 0L; 104L; 0L; 3L; 10L; 104L; 0L; 104L; 0L; 21101L; 0L; 983925826324L; 1L; 21101L; 0L; 427L; 0L; 1106L; 0L; 439L; 21101L; 988220642048L; 0L; 1L; 21102L; 1L; 438L; 0L; 1105L; 1L; 439L; 99L; 109L; 2L; 21201L; -1L; 0L; 1L; 21102L; 1L; 40L; 2L; 21101L; 0L; 470L; 3L; 21101L; 460L; 0L; 0L; 1106L; 0L; 503L; 109L; -2L; 2105L; 1L; 0L; 0L; 1L; 0L; 0L; 1L; 109L; 2L; 3L; 10L; 204L; -1L; 1001L; 465L; 466L; 481L; 4L; 0L; 1001L; 465L; 1L; 465L; 108L; 4L; 465L; 10L; 1006L; 10L; 497L; 1101L; 0L; 0L; 465L; 109L; -2L; 2106L; 0L; 0L; 0L; 109L; 4L; 2102L; 1L; -1L; 502L; 1207L; -3L; 0L; 10L; 1006L; 10L; 520L; 21101L; 0L; 0L; -3L; 22102L; 1L; -3L; 1L; 21202L; -2L; 1L; 2L; 21102L; 1L; 1L; 3L; 21102L; 1L; 539L; 0L; 1105L; 1L; 544L; 109L; -4L; 2106L; 0L; 0L; 109L; 5L; 1207L; -3L; 1L; 10L; 1006L; 10L; 567L; 2207L; -4L; -2L; 10L; 1006L; 10L; 567L; 21202L; -4L; 1L; -4L; 1106L; 0L; 635L; 21202L; -4L; 1L; 1L; 21201L; -3L; -1L; 2L; 21202L; -2L; 2L; 3L; 21102L; 1L; 586L; 0L; 1105L; 1L; 544L; 21202L; 1L; 1L; -4L; 21102L; 1L; 1L; -1L; 2207L; -4L; -2L; 10L; 1006L; 10L; 605L; 21101L; 0L; 0L; -1L; 22202L; -2L; -1L; -2L; 2107L; 0L; -3L; 10L; 1006L; 10L; 627L; 21202L; -1L; 1L; 1L; 21102L; 1L; 627L; 0L; 105L; 1L; 502L; 21202L; -2L; -1L; -2L; 22201L; -4L; -2L; -4L; 109L; -5L; 2106L; 0L; 0L |]

type Color = | Black = 0 | White = 1
type Direction = | Up | Right | Down | Left
type Robot = { position: int * int; direction: Direction }

module Robot =
    let create () = { position = (0, 0); direction = Up }

    let turnLeft robot =
        match robot.direction with
        | Up -> { robot with direction = Left }
        | Left -> { robot with direction = Down }
        | Down -> { robot with direction = Right }
        | Right -> { robot with direction = Up }

    let turnRight robot =
        match robot.direction with
        | Up -> { robot with direction = Right }
        | Right -> { robot with direction = Down }
        | Down -> { robot with direction = Left }
        | Left -> { robot with direction = Up }

    let turn instruction robot =
        match instruction with
        | 0 -> turnLeft robot
        | 1 -> turnRight robot
        | _ -> failwith "Invalid turn direction"

    let moveForward robot =
        let (x, y) = robot.position
        match robot.direction with
        | Up -> { robot with position = (x, y - 1) }
        | Right -> { robot with position = (x + 1, y) }
        | Down -> { robot with position = (x, y + 1) }
        | Left -> { robot with position = (x - 1, y) }

let calcPaintedCoords startingColor =
    let initialState = Program.create (Some <| Persistent 0L) input

    let rec calcPaintedCoords' program robot paintedCoords =
        let colorAtRobot = Map.tryFind robot.position paintedCoords |> Option.defaultValue Color.Black
        match Program.processOpCode { program with input = Some <| Persistent (int64 colorAtRobot) } with
        | Program.Output (output, program) ->
            let paintedCoords = Map.add robot.position (enum<Color> (int output)) paintedCoords
            match Program.processOpCode program with
            | Program.Output (output, program) ->
                let robot = Robot.turn (int output) robot |> Robot.moveForward
                calcPaintedCoords' program robot paintedCoords
            | Program.ProgramEnd -> paintedCoords
        | Program.ProgramEnd -> paintedCoords

    calcPaintedCoords' initialState (Robot.create ()) (Map.ofList [(0, 0), startingColor])


let part1 () =
    calcPaintedCoords Color.Black |> Map.count

let part2 () =
    let paintedCoords = Map.toList (calcPaintedCoords Color.White)
    let x1 = List.minBy (fun (coord, _) -> fst coord) paintedCoords |> fun (coord, _) -> fst coord
    let x2 = List.maxBy (fun (coord, _) -> fst coord) paintedCoords |> fun (coord, _) -> fst coord

    Seq.groupBy (fun (coord, _) -> snd coord) paintedCoords
    |> Seq.sortBy fst
    |> Seq.map (fun (y, row) ->
        seq { x1 .. x2 }
        |> Seq.map (fun x ->
            Seq.tryFind (fun (coord, _) -> coord = (x, y)) row
            |> Option.map (fun (_, color) -> if color = Color.Black then ' ' else '#')
            |> Option.defaultValue ' '
        )
        |> Seq.toArray
        |> String
    )
    |> List.ofSeq

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    printfn "%A" (part2 ())
    0 // return an integer exit code
