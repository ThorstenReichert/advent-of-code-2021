open System
open System.IO

[<EntryPoint>]
let main _ =
    let initialPopulation = 
        File.ReadAllLines "Input.txt"
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map (fun line -> line.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries))
        |> Seq.collect (Seq.map Int32.Parse)
        |> Seq.groupBy (id)
        |> Seq.map (fun (lifetime, fs) -> (lifetime, fs |> Seq.length |> int64))
        |> Seq.toList

    let updatePopulation (population: (int * int64) list) =
        population
        |> Seq.collect (fun (lifetime, count) -> 
            if lifetime > 0 then [(lifetime - 1, count)]
            else [(6, count); (8, count)])
        |> Seq.groupBy fst
        |> Seq.map (fun (lifetime, counts) -> (lifetime, counts |> Seq.sumBy snd))
        |> Seq.toList

    let observationTime = 256
    let populationCount =
        initialPopulation
        |> Seq.unfold (fun currentPopulation -> 
            let updatedPopulation = updatePopulation currentPopulation
            Some (updatedPopulation, updatedPopulation))
        |> Seq.item (observationTime - 1)
        |> Seq.sumBy (fun (_, count) -> count)

    printfn "population count after %d days = %d" observationTime populationCount

    0 