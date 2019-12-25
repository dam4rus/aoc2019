open System

let input = ".#..#..#..#...#..#...###....##.#....
.#.........#.#....#...........####.#
#..##.##.#....#...#.#....#..........
......###..#.#...............#.....#
......#......#....#..##....##.......
....................#..............#
..#....##...#.....#..#..........#..#
..#.#.....#..#..#..#.#....#.###.##.#
.........##.#..#.......#.........#..
.##..#..##....#.#...#.#.####.....#..
.##....#.#....#.......#......##....#
..#...#.#...##......#####..#......#.
##..#...#.....#...###..#..........#.
......##..#.##..#.....#.......##..#.
#..##..#..#.....#.#.####........#.#.
#......#..........###...#..#....##..
.......#...#....#.##.#..##......#...
.............##.......#.#.#..#...##.
..#..##...#...............#..#......
##....#...#.#....#..#.....##..##....
.#...##...........#..#..............
.............#....###...#.##....#.#.
#..#.#..#...#....#.....#............
....#.###....##....##...............
....#..........#..#..#.......#.#....
#..#....##.....#............#..#....
...##.............#...#.....#..###..
...#.......#........###.##..#..##.##
.#.##.#...##..#.#........#.....#....
#......#....#......#....###.#.....#.
......#.##......#...#.#.##.##...#...
..#...#.#........#....#...........#.
......#.##..#..#.....#......##..#...
..##.........#......#..##.#.#.......
.#....#..#....###..#....##..........
..............#....##...#.####...##."

let stringToMap (stringInput: string) =
    stringInput.Split('\n')
    |> Seq.map (fun line -> line.TrimEnd())
    |> Seq.indexed
    |> Seq.collect (fun (y, row) ->
        Seq.indexed row
        |> Seq.choose (fun (x, c) -> if c = '#' then Some (x, y) else None)
    )
    |> List.ofSeq

let asteroidMap = stringToMap input

let moveByVector pt vec = (fst pt + fst vec, snd pt + snd vec) 

let countVisibleAsteroids pt1 = 
    Seq.except (Seq.singleton pt1) asteroidMap
    |> Seq.filter (fun pt2 ->
        let (x1, y1) = pt1
        let (x2, y2) = pt2
        match (abs (x2 - x1), abs (y2 - y1)) with
        | (1, _) | (_, 1) -> true
        | (0, _) -> not <| Seq.exists (fun (x, y) -> x = x1 && y > min y1 y2 && y < max y1 y2) asteroidMap
        | (_, 0) -> not <| Seq.exists (fun (x, y) -> y = y1 && x > min x1 x2 && x < max x1 x2) asteroidMap
        | (absDeltaX, absDeltaY) ->
            let highestCommonDivisor =
                seq { for n in min absDeltaX absDeltaY .. -1 .. 1 do yield n }
                |> Seq.find(fun n -> absDeltaX % n = 0 && absDeltaY % n = 0)

            let vec = ((x2 - x1) / highestCommonDivisor, (y2 - y1) / highestCommonDivisor)
            Seq.unfold (fun pt -> if pt = pt2 then None else Some(pt, moveByVector pt vec)) (moveByVector pt1 vec)
            |> Seq.exists (fun pt -> Seq.contains pt asteroidMap)
            |> not
    )
    |> Seq.length

let part1 () =
    Seq.map countVisibleAsteroids asteroidMap
    |> Seq.max

type LaserTarget = (double * (int * int) list)
type LaserTargetList = LaserTarget list

let part2 () =
    let stationPosition = Seq.maxBy countVisibleAsteroids asteroidMap
    let (stationX, stationY) = stationPosition
    printfn "%A" stationPosition

    let targets =
        Seq.except (Seq.singleton stationPosition) asteroidMap
        |> Seq.sortBy (fun (x, y) -> abs (stationX - x) + abs (stationY - y))
        |> List.ofSeq
        |> List.groupBy (fun (x, y) ->
            match (atan2 (double y - double stationY) (double x - double stationX)) * 180.0 / Math.PI with
            | angle when angle < 0.0 -> angle + 360.0
            | angle -> angle
        )
        |> List.sortBy fst

    let (first, second) = List.splitAt (List.findIndex (fun (angle, _) -> angle >= 270.0) targets) targets
    let targets = second @ first
    let rec destroyAsteroids (remaining: LaserTargetList) destroyCount =
        match destroyCount + List.length remaining with
        | destroyed when destroyed >= 200 -> List.item (200 - destroyCount - 1) remaining
        | destroyed ->
            let remaining =
                Seq.filter (fun (_, targets) -> List.length targets > 1) remaining
                |> Seq.map (fun (angle, targets) -> (angle, List.tail targets))
                |> List.ofSeq

            destroyAsteroids remaining destroyed

    snd (destroyAsteroids targets 0)
    |> List.head
    |> fun (x, y) -> x * 100 + y

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    printfn "%A" (part2 ())
    0 // return an integer exit code
