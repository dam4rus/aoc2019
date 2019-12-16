open System.IO

let part1 input =
    let rec countOrbittingObjects accumulator level objectName =
        match Map.tryFind objectName input with
        | Some orbittingObjects -> accumulator + level + List.sumBy (countOrbittingObjects accumulator (level + 1)) orbittingObjects
        | None -> level

    countOrbittingObjects 0 0 "COM"

type Object = { name: string; prev: Object option; }

let part2 input =
    let rec collectObjects prev objectName =
        let object = { name = objectName; prev = prev }
        let siblings = Map.tryFind objectName input |> Option.map (List.collect (collectObjects <| Some object)) |> Option.defaultValue List.empty
        object :: siblings

    let objects = collectObjects None "COM"
    let santaOrbit = List.find (fun object -> object.name = "SAN") objects
    let myOrbit = List.find (fun object -> object.name = "YOU") objects

    let rec buildPath object =
        let prevObjects = Option.map buildPath object.prev |> Option.defaultValue List.empty
        object.name :: prevObjects

    let santaPath = (buildPath santaOrbit) |> List.rev
    let myPath = (buildPath myOrbit) |> List.rev
    let mutualPathLen =
        Seq.zip santaPath myPath
        |> Seq.takeWhile (fun (obj1, obj2) -> obj1 = obj2)
        |> Seq.length

    (List.length myPath - mutualPathLen) + (List.length santaPath - mutualPathLen) - 2

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines("input.txt")
        |> Seq.map (fun line ->
            let pair = line.Split(')')
            pair.[0], pair.[1]
        )
        |> Seq.fold (fun state (key, value) ->
            let listOfOrbitingObjects = value :: (Map.tryFind key state |> Option.defaultValue List.empty)
            state |> Map.add key listOfOrbitingObjects
        ) Map.empty

    printfn "%d" (part1 input)
    printfn "%d" (part2 input)
    0 // return an integer exit code
