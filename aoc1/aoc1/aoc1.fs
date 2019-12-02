module aoc1

open System
open System.IO

let massToFuel input = input / 3 - 2

let part1 = Array.sumBy (Int32.Parse >> massToFuel)
let part2 =
    let rec accumulator input =
        match massToFuel input with
        | result when result > 0 -> result + accumulator result
        | _ -> 0
        
    Array.sumBy (Int32.Parse >> accumulator)

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt"

    printf "%d\n" (part1 input)
    printf "%d\n" (part2 input)
    0