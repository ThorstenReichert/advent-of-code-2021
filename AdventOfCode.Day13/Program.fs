open System
open System.IO

[<EntryPoint>]
let main _ =
    let (|ParseInt|) text = Int32.Parse text
    let fold transform paper =
        paper
        |> Map.fold (fun dots (dx,dy) _ -> dots |> Map.add (transform (dx, dy)) ()) Map.empty

    let writeAt (text: string) (x,y) =
        Console.SetCursorPosition(x + 3, y + 3)
        Console.Write(text)

    let paper = 
        File.ReadAllLines "Input.txt"
        |> Seq.takeWhile (String.IsNullOrWhiteSpace >> not)
        |> Seq.map (fun line -> 
            let parts = line.Split(',')
            ((Int32.Parse parts.[0], Int32.Parse parts.[1]), ()))
        |> Map

    let folds =
        File.ReadAllLines "Input.txt"
        |> Seq.skipWhile (String.IsNullOrWhiteSpace >> not)
        |> Seq.skip 1
        |> Seq.scan (fun curr instruction ->
            match instruction.Split('=') with
            | [|"fold along x"; ParseInt x|] -> curr |> fold (fun (dx,dy) -> (x - Math.Abs(x - dx), dy))
            | [|"fold along y"; ParseInt y|] -> curr |> fold (fun (dx,dy) -> (dx, y - Math.Abs(dy - y)))
            | _ -> failwith "unrecognized instruction") paper
        |> Seq.toList

    let dotsAfterFirstFold = folds |> Seq.item 1 |> Map.count
    let activationCode = folds |> Seq.last

    printfn "dots after first fold = %d" dotsAfterFirstFold
    activationCode
    |> Seq.iter (fun dot -> writeAt "#" dot.Key)

    printfn ""
    printfn ""

    0