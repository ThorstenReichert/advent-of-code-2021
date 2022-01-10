open System
open System.IO

let updateBoard draw board =
    match board |> Map.tryPick (fun pos (guess, _) -> if guess = draw then Some (pos, guess) else None) with
    | Some (pos, guess) -> board |> Map.add pos (guess, true)
    | None -> board

let isWinningBoard board =
    let hasWinningRow = 
        [0..4]
        |> Seq.map (fun row ->
            [0..4]
            |> Seq.map (fun col -> board |> Map.find (row, col) |> snd)
            |> Seq.forall id)
        |> Seq.exists id

    let hasWinningColumn =
        [0..4]
        |> Seq.map (fun col ->
            [0..4]
            |> Seq.map (fun row -> board |> Map.find (row, col) |> snd)
            |> Seq.forall id)
        |> Seq.exists id
    
    hasWinningColumn || hasWinningRow
        
let calculateWinningSequence (draws: int[]) boards =
    let rec loop round boards =
        let winning = boards |> List.filter (isWinningBoard)
        let remaining = boards |> List.filter (isWinningBoard >> not)

        if round >= draws.Length then [round - 1, winning]
        else
            (round - 1, winning |> Seq.toList)
            :: (loop (round + 1) (remaining |> List.map (updateBoard (draws.[round]))))

    loop 0 (boards |> Seq.toList)
    |> List.filter (snd >> Seq.isEmpty >> not)

let chooseWinningBoard draws boards selector =
    let (round, board) =
        calculateWinningSequence draws boards
        |> selector
        |> (fun (round, boards) -> (round, boards |> Seq.exactlyOne))

    let score = 
        board 
        |> Map.toSeq 
        |> Seq.map (fun (_, (guess, drawn)) -> if drawn then 0 else guess)
        |> Seq.sum

    (round, score)

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "Input.txt"
    let draws = 
        input.[0].Split(',')
        |> Array.map Int32.Parse

    let boards =
        input
        |> Seq.skip 1
        |> Seq.chunkBySize 6
        |> Seq.map (fun chunk -> 
            chunk
            |> Seq.skip 1
            |> Seq.mapi (fun x line -> 
                line.Split(" ", StringSplitOptions.RemoveEmptyEntries) 
                |> Seq.mapi (fun y entry -> ((x,y), (Int32.Parse entry, false))))
            |> Seq.collect id
            |> Map)
        |> Seq.toArray

    let (round, score) = chooseWinningBoard draws boards Seq.head
    let lastDraw = draws.[round]

    printfn "last round    = %d" round
    printfn "last draw     = %d" lastDraw
    printfn "winning score = %d" score
    printfn "solution      = %d" (score * lastDraw)

    printfn ""

    let (round, score) = chooseWinningBoard draws boards Seq.last
    let lastDraw = draws.[round]

    printfn "last round    = %d" round
    printfn "last draw     = %d" lastDraw
    printfn "loosing score = %d" score
    printfn "solution      = %d" (score * lastDraw)

    0 