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

let testInput1 = ".#..#
.....
#####
....#
...##"

let rec countVisibleAsteroids remainingObjects map =
    match remainingObjects with
    | [] -> []
    | pt1 :: rest ->
        let visibleCount =
            Seq.filter ((<>) pt1) map
            |> Seq.fold (fun acc pt2 ->
                let (x1, y1) = pt1
                let (x2, y2) = pt2
                if x1 = x2 && y1 < y2 then
                    if Seq.exists (fun (x, y) -> x = x1 && y > y1 && y < y2) map then acc else acc + 1
                else if x1 = x2 && y1 > y2 then
                    if Seq.exists (fun (x, y) -> x = x1 && y < y1 && y > y2) map then acc else acc + 1
                else if y1 = y2 && x1 < x2 then
                    if Seq.exists (fun (x, y) -> y = y1 && x > x1 && x < x2) map then acc else acc + 1
                else if y1 = y2 && x1 > x2 then
                    if Seq.exists (fun (x, y) -> y = y1 && x < x1 && x > x2) map then acc else acc + 1
                else
                    let absDeltaX = abs (x2 - x1)
                    let absDeltaY = abs (y2 - y1)
                    if absDeltaX = 1 || absDeltaY = 1 then
                        acc + 1
                    else
                        let highestCommonDivisor =
                            seq { for n in min absDeltaX absDeltaY .. -1 .. 1 do yield n }
                            |> Seq.find(fun n -> absDeltaX % n = 0 && absDeltaY % n = 0)

                        let vec = ((x2 - x1) / highestCommonDivisor, (y2 - y1) / highestCommonDivisor)
                        let iterator = Seq.unfold (fun pt -> if pt = pt2 then None else Some(pt, (fst pt + fst vec, snd pt + snd vec))) (x1 + fst vec, y1 + snd vec)
                        if Seq.exists (fun pt -> Seq.contains pt map) iterator then acc else acc + 1
            ) 0

        visibleCount :: countVisibleAsteroids rest map

let stringToMap (stringInput: string) =
    stringInput.Split('\n')
    |> Seq.map (fun line -> line.TrimEnd())
    |> Seq.indexed
    |> Seq.collect (fun (y, row) ->
        Seq.indexed row
        |> Seq.choose (fun (x, c) -> if c = '#' then Some (x, y) else None)
    )
    |> List.ofSeq

let part1 () =
    let map = stringToMap input
    countVisibleAsteroids map map
    |> List.max

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    0 // return an integer exit code
