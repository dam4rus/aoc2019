let passwordMin = 145852
let passwordMax = 616942

let digitsToInt = Seq.rev >> Seq.indexed >> Seq.sumBy (fun (index, n) -> n * pown 10 index)

let stepDigits digits =
    match List.tail digits |> List.tryFindIndex ((=) 9) with
    | Some index -> List.take index digits @ List.init (6 - index) (fun _ -> (List.item index digits) + 1)
    | None -> List.take 5 digits @ [ List.last digits + 1 ]

let part1 () =
    let hasPairwiseMatch = Seq.pairwise >> Seq.exists (fun (first, second) -> first = second)

    let rec iterate digits count =
        match digitsToInt digits with
        | n when n < passwordMax -> iterate (stepDigits digits) (if hasPairwiseMatch digits then count + 1 else count)
        | _ -> count

    iterate [ 1; 4; 5; 8; 8; 8 ] 0

let part2 () =
    let rec onlyPairwise = function
        | first :: second :: [] -> first = second
        | first :: second :: third :: _ when first = second && first <> third -> true
        | head :: rest -> onlyPairwise <| List.skipWhile ((=) head) rest
        | [] -> false

    let rec iterate digits count =
        match digitsToInt digits with
        | n when n < passwordMax ->
            
            iterate (stepDigits digits) (if onlyPairwise digits then count + 1 else count)
        | _ -> count

    iterate [ 1; 4; 5; 8; 8; 8 ] 0

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    printfn "%d" (part2 ())
    0 // return an integer exit code
