open System
open System.IO
open Zipper

let rec parse (text: char list) =
    let (|Digit|_|) c =
        if Char.IsDigit(c) then (int64 c) - (int64 '0') |> Some 
        else None

    let findClosure text =
        let rec loop level remaining =
            match remaining with
            | ']' :: _ when level = 0 -> []
            | ']' :: tail -> ']' :: loop (level-1) tail
            | '[' :: tail -> '[' :: loop (level + 1) tail
            | x :: tail -> x :: loop level tail
            | _ -> failwith "invalid format (no matching bracket found)"

        loop 0 text

    let split text =
        let rec loop level remaining =
            match remaining with
            | '[' :: tail -> '[' :: loop (level + 1) tail
            | ']' :: tail -> ']' :: loop (level - 1) tail
            | ',' :: _ when level = 0 -> []
            | x :: tail -> x :: loop level tail
            | _ -> failwith "invalid format (no separator found)"

        let left = loop 0 text
        let right = text |> List.skip ((left |> List.length) + 1)

        (left, right)

    match text with
    | (Digit d) :: _ -> Leaf d
    | '['::tail -> 
        let closure = findClosure tail
        let (left, right) = split closure

        Node (parse left, parse right)
    | _ -> failwith "invalid format"
    
let rec toString number =
    match number with
    | Leaf x -> x.ToString()
    | Node (l, r) -> "[" + (toString l) + "," + (toString r) + "]"

let toNumber tz =
    let (TreeZipper (t, _)) = top tz in t

let rec explode tz =
    let leftValue = tz |> left |> current
    let rightValue = tz |> right |> current

    tz
    |> replace (unit 0L)
    |> (fun tz ->
        match tz |> tryPrevLeaf with
        | None -> tz
        | Some tz -> tz |> mapSub ((+) leftValue) |> nextLeaf)
    |> (fun tz ->
        match tz |> tryNextLeaf with
        | None -> tz
        | Some tz -> tz |> mapSub ((+) rightValue) |> prevLeaf)

and split (tz: TreeZipper<int64>) =
    let value = tz |> current
    let (half, remainder) = Math.DivRem (value, 2L)
    let leftValue = half
    let rightValue = half + remainder

    let result =
        tz 
        |> replace (Node (Leaf leftValue, Leaf rightValue) |> zipper)

    result

let reduce number =
    let rec loop tz =
        let isSplitTarget tz = 
            current tz > 9L
    
        let isExplodeTarget tz =
            match tryUp tz with
            | None -> false
            | Some parent -> depth parent >= 4

        let rec nextExplode tz =
            if isExplodeTarget tz then
                (true, tz |> up |> explode)
            else
                match tryNextLeaf tz with
                | Some tz -> nextExplode tz
                | None -> (false, tz)

        let rec nextSplit tz =
            if isSplitTarget tz then
                (true, tz |> split)
            else
                match tryNextLeaf tz with
                | Some tz -> nextSplit tz
                | None -> (false, tz)

        let start = leftLeaf tz
        let (modified, tz) = nextExplode start
        if modified then
            loop (top tz)
        else 
            let (modified, tz) = nextSplit start
            if modified then
                loop (top tz)
            else
                let (TreeZipper (t, _)) = top tz in t

    loop (zipper number)

let add left right =
    Node (left, right) |> reduce

let rec magnitude number =
    match number with
    | Node (left, right) -> 3L * (magnitude left) + 2L * (magnitude right)
    | Leaf number -> number

[<EntryPoint>]
let main _ =
    let input =
        File.ReadAllLines "Input.txt"
        |> Array.map (Seq.toList >> parse)

    let sum =
        input
        |> Seq.reduce add

    printfn "result    = %s" (toString sum)
    printfn "magnitude = %d" (magnitude sum)

    let largestSum =
        seq {
            for i in 0 .. (input.Length - 1) do
                for j in 0 .. (input.Length - 1) do
                    if i <> j then yield (input.[i], input.[j])
        }
        |> Seq.map (fun (x,y) -> add x y)
        |> Seq.maxBy magnitude

    printfn ""
    printfn "largest sum = %s" (toString largestSum)
    printfn "magnitude   = %d" (magnitude largestSum)

    0