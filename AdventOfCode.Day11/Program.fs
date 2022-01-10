open System.IO

let getNeighbours map (x, y)=
    seq { (x+1, y); (x+1, y+1); (x, y+1); (x-1, y+1); (x-1,y); (x-1, y-1); (x, y-1); (x+1, y-1) }
    |> Seq.choose (fun pos -> 
        match map |> Map.tryFind pos with
        | Some value -> Some (pos, value)
        | None -> None)

[<EntryPoint>]
let main _ =
    let input =
        File.ReadAllLines "Input.txt"
        |> Seq.mapi (fun x line -> 
            line
            |> Seq.mapi (fun y charge -> ((x, y), (int charge - int '0'))))
        |> Seq.collect id
        |> Map

    let nextStep curr =
        let updated = curr |> Map.map (fun _ charge -> charge + 1)

        let rec flash map (pos, charge) =
            getNeighbours map pos
            |> Seq.fold 
                (fun m (p, _) ->
                    match m |> Map.tryFind p with
                    | Some 9 -> flash m (p, 9)                      // flash neighbour if neighbour charge is exactly 9
                    | Some charge -> m |> Map.add p (charge + 1)    // otherwise +1 (let charge run above 10)
                    | None -> m)
                (map |> Map.add pos (charge + 1))

        updated
        |> Seq.filter (fun octo -> octo.Value > 9)
        |> Seq.fold (fun map octo -> flash map (octo.Key, octo.Value)) updated
        |> (fun map -> 
            (map |> Seq.filter (fun octo -> octo.Value > 9)) |> Seq.length,
             map |> Map.map (fun _ charge -> if charge > 9 then 0 else charge))
            
    let states = 
        Seq.unfold (fun (step, flashes, map) ->
            let (newFlashes, updatedMap) = nextStep map
            let update = (step + 1, flashes + newFlashes, updatedMap)

            Some (update, update))
            (0, 0, input)

    let steps = 100
    let (_, totalFlashes, _) = states |> Seq.item (steps-1)

    let firstSimultaneousFlash =
        states
        |> Seq.pick (fun (step, _, map) -> 
            if (map |> Seq.filter (fun octo -> octo.Value = 0) |> Seq.length) = (map |> Map.count) then Some step
            else None)
    
    printfn "total flashes after %d steps = %d" steps totalFlashes
    printfn "first simultaneous flash     = %d" firstSimultaneousFlash

    0