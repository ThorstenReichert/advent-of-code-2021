// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let tally (lines: string[]) = 
    lines 
    |> Seq.fold 
        (fun tallies line -> 
            tallies |> Array.mapi (fun index total -> 
                match line.[index] with
                | '1' -> total + 1
                | '0' -> total - 1
                | _ -> failwith "unexpected token"))
        (Array.zeroCreate lines.[0].Length)

let calculateLifeSupportRating (input: string[]) inputFilterPredicate =
    let rec loop input (pos: int) =
        let tallies = tally input
        let remaining =
            input
            |> Array.filter (fun line -> inputFilterPredicate tallies.[pos] line.[pos])

        match remaining.Length with
        | 0 -> failwith "no input line matches filter criterium"
        | 1 -> remaining.[0]
        | _ -> loop remaining (pos + 1)
    
    loop input 0
    |> (fun rating -> Convert.ToInt32(rating, 2))

let calculatePowerRating input bitSelector =
    tally input
    |> Array.map bitSelector
    |> Array.reduce (+)
    |> (fun rating -> Convert.ToInt32(rating, 2))

[<EntryPoint>]
let main _ =
    let input = 
        File.ReadAllLines "Input.csv"
        |> Array.filter(String.IsNullOrWhiteSpace >> not)

    let gamma = calculatePowerRating input (fun tally -> 
        if tally > 0 then "1"
        else if tally < 0 then "0"
        else failwith "equal tally")

    let epsilon = calculatePowerRating input (fun tally -> 
        if tally > 0 then "0"
        else if tally < 0 then "1"
        else failwith "equal tally")

    let oxygenGeneratorRating = calculateLifeSupportRating input (fun tally bit ->
        (tally >= 0 && bit = '1') || (tally < 0 && bit = '0'))

    let co2ScrubberRating = calculateLifeSupportRating input (fun tally bit ->
        (tally >= 0 && bit = '0') || (tally < 0 && bit = '1'))

    printfn "gamma rating            = %d" gamma
    printfn "epsilon rating          = %d" epsilon
    printfn "solution1               = %d" (gamma * epsilon)

    printfn ""

    printfn "oxygen generator rating = %d" oxygenGeneratorRating
    printfn "co2 scrubber rating     = %d" co2ScrubberRating
    printfn "solution2               = %d" (oxygenGeneratorRating * co2ScrubberRating)

    0 // return an integer exit code