// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO;

let (|Increase|Decrease|Equal|) (prev, curr) =
    if curr > prev then Increase
    else if curr < prev then Decrease
    else Equal

let printChanges (prev, curr) =
    printfn "%i (%s)" curr (match (prev, curr) with
        | Increase -> "Increase"
        | Decrease -> "Decrease"
        | Equal -> "Equal")

    (prev, curr)

let triples (source: 'a seq) =
    source
    |> Seq.pairwise
    |> Seq.pairwise
    |> Seq.map (fun ((a,b), (_,d)) -> (a,b,d))

[<EntryPoint>]
let main argv =
    
    let increases = 
        File.ReadAllLines("Input.csv")
        |> Array.toSeq
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map Int32.Parse
        |> triples
        |> Seq.map(fun (x,y,z) -> x + y + z)
        |> Seq.pairwise
        |> Seq.map printChanges
        |> Seq.map (fun (prev, curr) -> match (prev, curr) with
            | Increase -> 1
            | Decrease -> 0
            | Equal -> 0)
        |> Seq.sum

    printfn ""
    printfn "Found %i increases" increases
    0