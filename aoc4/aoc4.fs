let passwordMin = 145852
let passwordMax = 616942

let digitsToInt = Seq.rev >> Seq.indexed >> Seq.sumBy (fun (index, n) -> n * pown 10 index)

let stepDigits digits =
    match List.tail digits |> List.tryFindIndex ((=) 9) with
    | Some index -> List.take index digits @ List.init (6 - index) (fun _ -> (List.item index digits) + 1)
    | None -> List.take 5 digits @ [ List.last digits + 1 ]

let rec countValidPasswordsBy digits count (matcher: (int list -> bool)) =
    match digitsToInt digits with
    | n when n < passwordMax -> countValidPasswordsBy (stepDigits digits) (if matcher digits then count + 1 else count) matcher
    | _ -> count

let part1 () =
    let hasPairwiseMatch = Seq.pairwise >> Seq.exists (fun (first, second) -> first = second)
    countValidPasswordsBy [ 1; 4; 5; 8; 8; 8 ] 0 hasPairwiseMatch

let part2 () =
    let rec onlyPairwise = function
        | first :: [ second ] -> first = second
        | first :: second :: third :: _ when first = second && first <> third -> true
        | head :: rest -> onlyPairwise <| List.skipWhile ((=) head) rest
        | [] -> false

    countValidPasswordsBy [ 1; 4; 5; 8; 8; 8 ] 0 onlyPairwise

[<EntryPoint>]
let main argv =
    printfn "%d" (part1 ())
    printfn "%d" (part2 ())
    0 // return an integer exit code
