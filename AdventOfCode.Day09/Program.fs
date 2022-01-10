open System
open System.IO

[<EntryPoint>]
let main _ =
    let getNeighbours map (x,y) =
        seq { (x+1, y); (x-1, y); (x, y+1); (x, y-1) }
        |> Seq.choose (fun pos -> 
            match Map.tryFind pos map with
            | Some height -> Some (pos, height)
            | None -> None)

    let isHotspot map pos =
        match Map.tryFind pos map with
        | Some height -> (getNeighbours map pos) |> Seq.map snd |> Seq.forall ((<) height)
        | None -> failwith "invalid map position"

    let findBasinOfHotspot map hotspot =
        let rec loop basinMap pos =
            match (basinMap |> Map.tryFind pos, map |> Map.tryFind pos) with
            | (_, None)                     // outside of map
            | (Some _, _)                   // already marked as part of basin
            | (None, Some 9) -> basinMap    // height 9 places are never part of basins
            | (None, Some height) ->        // update basinMap and recurse over all neighbours
                getNeighbours map pos
                |> Seq.fold 
                    (fun bm (pos, _) -> loop bm pos)
                    (basinMap |> Map.add pos height)

        loop Map.empty hotspot

    let map =
        File.ReadAllLines "Input.txt"
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.mapi (fun row line -> 
            line
            |> Seq.mapi (fun col digit -> ((row, col), Int32.Parse (digit.ToString()))))
        |> Seq.collect id
        |> Map

    let hotspots =
        map 
        |> Seq.filter (fun entry -> isHotspot map entry.Key)
        |> Seq.map (fun entry -> (entry.Key, entry.Value))
        |> Seq.toArray
        
    let risk =
        hotspots
        |> Seq.sumBy (snd >> (+) 1)

    let largestBasins = 
        hotspots
        |> Seq.map fst
        |> Seq.map (findBasinOfHotspot map)
        |> Seq.sortByDescending Seq.length
        |> Seq.take 3
        |> Seq.toArray

    let solution =
        largestBasins
        |> Seq.map Seq.length
        |> Seq.reduce (*)


    printfn "risk = %d" risk
    printfn "solution = %d" solution

    0 