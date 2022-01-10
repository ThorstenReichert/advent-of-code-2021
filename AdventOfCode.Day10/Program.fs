open System.IO

[<EntryPoint>]
let main argv =
    let parens = [| ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') |]
    let mismatchedScores = Map [ (')', 3L); (']', 57L); ('}', 1197L); ('>', 25137L) ]
    let unmatchedScores = Map [ ('(', 1L); ('[', 2L); ('{', 3L); ('<', 4L) ]

    let scoreMismatched mismatched =
        mismatched |> Seq.last |> (fun c -> Map.find c mismatchedScores)

    let scoreUnmatched unmatched =
        unmatched |> Seq.fold (fun total curr -> total * 5L + (Map.find curr unmatchedScores)) 0L

    let lowerMedian source =
        source
        |> Seq.sort
        |> Seq.item ((Seq.length source) / 2)

    let isOpenParen c = parens |> Seq.map fst |> Seq.contains c
    let isMatch left right  = 
        match parens |> Array.tryFind (fun (o, _) -> o = left) with
        | Some (_, c) -> c = right
        | _ -> false

    let parenthesisMatches = 
        File.ReadAllLines "Input.txt"
        |> Seq.map (fun line ->
            line
            |> Seq.fold (fun (unmatched, mismatched) curr ->
                if isOpenParen curr then (curr :: unmatched, mismatched)
                else
                    match unmatched with
                    | [] -> (unmatched, curr :: mismatched)
                    | o :: rest -> 
                        if isMatch o curr then (rest, mismatched)
                        else (unmatched, curr :: mismatched)) ([], []))
        |> Seq.toArray

    let errorScore = 
        parenthesisMatches
        |> Seq.filter (snd >> List.isEmpty >> not)
        |> Seq.map (snd >> scoreMismatched)
        |> Seq.sum

    let completionScore =
        parenthesisMatches
        |> Seq.filter (snd >> List.isEmpty)
        |> Seq.map (fst >> scoreUnmatched)
        |> Seq.toArray
        |> lowerMedian

    printfn "error score      = %d" errorScore
    printfn "completion score = %d" completionScore

    0