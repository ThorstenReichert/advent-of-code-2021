open System
open System.IO

type LineType =
| Vertical
| Horizontal
| Diagonal

let parseLine (text: string) =
    let parts = text.Split([|'-'; '>'; ' '; ','|], StringSplitOptions.RemoveEmptyEntries)
    let (x1,y1,x2,y2) = (Int32.Parse parts.[0], Int32.Parse parts.[1], Int32.Parse parts.[2], Int32.Parse parts.[3])

    if x1 = x2 then (Vertical, [Math.Min(y1,y2)..Math.Max(y1,y2)] |> List.map (fun y -> (x1,y)))
    else if y1 = y2 then (Horizontal, [Math.Min(x1,x2)..Math.Max(x1,x2)] |> List.map (fun x -> (x,y1)))
    else if Math.Abs(x1-x2) = Math.Abs(y1-y2) then 
        let (stepx, stepy) = (Math.Sign(x2-x1), Math.Sign(y2-y1))
        (Diagonal, [0..Math.Abs(x1-x2)] |> List.map (fun i -> (x1 + i * stepx, y1 + i * stepy)))
    else failwith "general lines are not supported"

let updateMapWithLine map (lineType, pos) =
    pos
    |> List.fold (fun map pos -> 
        match map |> Map.tryFind pos with
        | Some z -> map |> Map.add pos (z+1)
        | None -> map |> Map.add pos 1) map

[<EntryPoint>]
let main argv =
    let map = 
        File.ReadAllLines "Input.txt"
        |> Seq.map parseLine
        |> Seq.filter (fun (lineType, _) -> 
            match lineType with 
            | Diagonal
            | Horizontal
            | Vertical _ -> true)
        |> Seq.fold updateMapWithLine Map.empty

    let hotspots = 
        map
        |> Map.toSeq
        |> Seq.filter (fun (_, overlaps) -> overlaps >= 2)
        |> Seq.length

    printfn "hotspots = %d" hotspots

    0