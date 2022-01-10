open System
open System.IO

let split (separator: char) (text: string) = 
    text.Split(separator, StringSplitOptions.RemoveEmptyEntries)

let rec distribute e = function
| [] -> [[e]]
| (x::xs') as l -> (e::l)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs)
    
let tryReadPattern (pattern: string) =
    match pattern |> Seq.sort |> String.Concat with
    | "abcefg" -> ValueSome "0"
    | "cf" -> ValueSome "1"
    | "acdeg" -> ValueSome "2"
    | "acdfg" -> ValueSome "3"
    | "bcdf" -> ValueSome "4"
    | "abdfg" -> ValueSome "5"
    | "abdefg" -> ValueSome "6"
    | "acf" -> ValueSome "7"
    | "abcdefg" -> ValueSome "8"
    | "abcdfg" -> ValueSome "9"
    | _ -> ValueNone
    
let applyMapping (mapping: char[]) pattern =
    pattern
    |> Seq.map (fun segment -> mapping.[(int segment) - (int 'a')])
    |> String.Concat
    
let isMappingConsistent patterns mapping =
    patterns
    |> Seq.map (applyMapping mapping)
    |> Seq.map tryReadPattern
    |> Seq.forall (function | ValueSome _ -> true | ValueNone -> false)

let selectMapping mappings patterns =
    mappings
    |> Seq.filter (isMappingConsistent patterns)
    |> Seq.exactlyOne

let readDisplay mapping display =
    display
    |> Seq.map (applyMapping mapping >> tryReadPattern >> ValueOption.get)
    |> String.Concat
    |> Convert.ToInt32

[<EntryPoint>]
let main _ =
    let input = 
        File.ReadAllLines "Input.txt"
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map (split '|')
        |> Seq.map (function 
            | [|signalPatterns; outputValues|] -> (split ' ' signalPatterns, split ' ' outputValues)
            | _ -> failwith "malformed input")
        |> Seq.toArray

    let uniquelyIdentifyablePatterns =
        let segmentCounts = [|2; 4; 3; 7|] // corresponds to number 1; 4; 7; 8
        input
        |> Seq.sumBy (fun (_, outputs) ->
            outputs
            |> Seq.filter (fun output -> Array.contains output.Length segmentCounts)
            |> Seq.length)

    let wireMappings = permute ("abcdefg" |> Seq.toList) |> List.map List.toArray
    let sumOfReadings =
        input
        |> Seq.map (fun (signals, outputs) -> (selectMapping wireMappings signals, outputs))
        |> Seq.map (fun (wireMapping, outputs) -> readDisplay wireMapping outputs)
        |> Seq.sum

    printfn "uniquely identifyable patterns = %d" uniquelyIdentifyablePatterns
    printfn "sum of readings                = %d" sumOfReadings

    0 