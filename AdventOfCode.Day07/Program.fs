open System
open System.IO

[<EntryPoint>]
let main argv =
    let crabs = 
        File.ReadAllLines "Input.txt"
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.collect (fun line -> line.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse)
        |> Seq.toArray

    let linearFuelConsumption (current: int) (target: int) =
        Math.Abs(target - current)

    let arithmeticcFuelConsumption (current: int) (target: int) =
        let distance = Math.Abs(target - current)
        (distance * distance + distance) / 2

    let (optimalPos, fuelConsumption) = 
        [(crabs |> Array.min)..(crabs |> Array.max)]
        |> Seq.map (fun pos -> 
            (pos, crabs |> Seq.sumBy (fun crab -> arithmeticcFuelConsumption crab pos)))
        |> Seq.minBy snd

    printfn "optimal position = %d" optimalPos
    printfn "fuel consumption = %d" fuelConsumption

    0