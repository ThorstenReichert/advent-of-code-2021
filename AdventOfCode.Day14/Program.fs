open System.IO

let setOrAddTo key value map =
    map |> Map.change key (fun x ->
        match x with
        | Some v -> Some (v + value)
        | None -> Some value)

type Polymer = {
    Monomers: Map<char, int64>
    Pairs : Map<char*char, int64> }

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "Input.txt"
    let insertions = input |> Seq.skip 2 |> Seq.map (fun line -> ((line.[0], line.[1]), line.[6])) |> Map
    let monomers = input |> Seq.head |> Seq.fold (fun counts monomer -> counts |> setOrAddTo monomer 1L) Map.empty
    let monomerPairs = input |> Seq.head |> Seq.pairwise |> Seq.fold (fun counts pair -> counts |> setOrAddTo pair 1L) Map.empty

    let apply insertions polymer =
        polymer.Pairs
        |> Map.fold (fun polymer (left, right) count -> 
            match insertions |> Map.tryFind (left, right) with
            | Some insertion -> 
                { Pairs = 
                    polymer.Pairs 
                    |> setOrAddTo (left, insertion) count 
                    |> setOrAddTo (insertion, right) count; 
                  Monomers = 
                    polymer.Monomers 
                    |> setOrAddTo insertion count }
            | None -> polymer) { polymer with Pairs = Map.empty }

    let polymerStatistics template insertions stages =
        [1..stages]
        |> Seq.fold (fun polymer _ -> apply insertions polymer) template
        |> (fun polymer -> (polymer.Monomers |> Map.toSeq |> Seq.minBy snd, polymer.Monomers |> Map.toSeq |> Seq.maxBy snd))

    let ((leastCommonElement, leastCommonCount), (mostCommonElement, mostCommonCount)) =
        polymerStatistics { Monomers = monomers; Pairs = monomerPairs } insertions 10

    printfn ""
    printfn "after 10 steps"
    printfn "least common element = %c; count = %d" leastCommonElement leastCommonCount
    printfn "most common element  = %c; count = %d" mostCommonElement mostCommonCount
    printfn "difference           = %d" (mostCommonCount - leastCommonCount)

    let ((leastCommonElement, leastCommonCount), (mostCommonElement, mostCommonCount)) =
        polymerStatistics { Monomers = monomers; Pairs = monomerPairs } insertions 40
        
    printfn ""
    printfn "after 40 steps"
    printfn "least common element = %c; count = %d" leastCommonElement leastCommonCount
    printfn "most common element  = %c; count = %d" mostCommonElement mostCommonCount
    printfn "difference           = %d" (mostCommonCount - leastCommonCount)

    0