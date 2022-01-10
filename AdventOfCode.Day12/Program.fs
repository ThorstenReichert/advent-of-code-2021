open System.IO

type Cave =
| Start
| End
| Large of string
| Small of string

[<EntryPoint>]
let main _ =
    let parseCave text =
        match text with
        | "start" -> Start
        | "end" -> End
        | cave when cave = cave.ToLower() -> Small cave
        | cave when cave = cave.ToUpper() -> Large cave
        | _ -> failwith "cannot parse cave"

    let connections = 
        File.ReadAllLines "Input.txt"
        |> Array.map (fun line ->
            let parts = line.Split('-')
            (parseCave parts.[0], parseCave parts.[1]))
    let connections = Array.append connections (connections |> Array.map (fun (s, e) -> (e, s)))

    let reachesEnd path = 
        match path with
        | End :: _ -> true
        | _ -> false

    let pathDidNotVisitSmallCave cave path =
        path |> List.contains (Small cave) |> not

    let pathHasNoDuplicateSmallCave path =
        path
        |> Seq.choose (function | Small cave -> Some (cave) | _ -> None)
        |> Seq.countBy id
        |> Seq.exists (fun (_, count) -> count > 1)
        |> not

    let rec continuations path allowVisit =
        let curr = List.head path

        if curr = End then Seq.singleton path
        else
            connections
            |> Seq.filter (fun (s, _) -> s = curr)
            |> Seq.filter (fun (_, e) -> allowVisit path e)
            |> Seq.collect (fun (_, e) -> continuations (e :: path) allowVisit)

    let limitedPaths = 
        continuations [Start] (fun path cave ->
            match cave with
            | Small cave -> path |> pathDidNotVisitSmallCave cave
            | Start -> false
            | _ -> true)
        |> Seq.filter reachesEnd

    let extendedPaths =
        continuations [Start] (fun path cave ->
            match cave with
            | Small cave -> (path |> pathDidNotVisitSmallCave cave) || (path |> pathHasNoDuplicateSmallCave)
            | Start -> false
            | _ -> true)
        |> Seq.filter reachesEnd
        

    printfn "number of limited paths  = %d" (limitedPaths |> Seq.length)
    printfn "number of extended paths = %d" (extendedPaths |> Seq.length)
    
    0