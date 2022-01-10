// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO;

type Movement =
| Forward of int
| Up of int
| Down of int

let parseLine (line: string) =
    let parts = line.Split " "
    let magnitude = Int32.Parse parts.[1]

    match parts.[0].ToLower() with
    | "forward" -> Forward magnitude
    | "up" -> Up magnitude
    | "down" -> Down magnitude
    | _ -> failwith ("Unrecognized direction " + parts.[0])

let interpret1 source =
    source
    |> Seq.fold (fun (pos,depth) m -> match m with
        | Forward f -> (pos + f, depth)
        | Up u -> (pos, depth - u)
        | Down d -> (pos, depth + d)) (0,0)

let interpret2 source =
    source
    |> Seq.fold (fun (pos,depth, aim) m -> match m with
        | Forward f -> (pos + f, depth + aim * f, aim)
        | Up u -> (pos, depth, aim - u)
        | Down d -> (pos, depth, aim + d)) (0,0,0)

[<EntryPoint>]
let main argv =
    let finalPosition = 
        File.ReadAllLines "Input.csv"
        |> Array.toSeq
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map parseLine
        |> interpret2

    printfn "Final Position = %A" finalPosition

    let (pos, depth, _) = finalPosition
    printfn "Solution = %A" (pos * depth)

    0 // return an integer exit code