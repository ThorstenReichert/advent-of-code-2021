open System.IO
open System
open System.Collections.Immutable
open System.Collections.Generic

type Loc = ValueTuple<int, int>

let inline create x y = Loc(x,y)
let inline shiftX ((x,y): Loc) shift = Loc(x + shift, y)
let inline shiftY ((x,y): Loc) shift = Loc(x, y + shift)
let inline getX ((x,_): Loc) = x
let inline getY ((_,y): Loc) = y

//[<CustomEquality>]
//[<CustomComparison>]
//type Loc = 
//    { X: int; Y: int }

//    interface IEquatable<Loc> with
//        member this.Equals other = other.X = this.X && other.Y = this.Y


//    override this.Equals other =
//        match other with
//        | :? Loc as loc -> (this :> IEquatable<_>).Equals(loc)
//        | _ -> false

//    override this.GetHashCode () =
//        HashCode.Combine(this.X, this.Y)

//    interface IComparable with
//        member this.CompareTo other =
//            match other with
//            | :? Loc as loc -> (this :> IComparable<_>).CompareTo loc
//            | _ -> -1

//    interface IComparable<Loc> with
//        member this.CompareTo other =
//            if this.X > other.X then 1
//            else if this.X < other.X then -1
//            else if this.Y > other.Y then 1
//            else if this.Y < other.Y then -1
//            else 0

//let inline create x y = { X = x; Y = y }
//let inline shiftX loc shift = { loc with X = loc.X + shift }
//let inline shiftY loc shift = { loc with Y = loc.Y + shift }

let dijkstra (distance: Loc -> Loc -> int) (neighbours: Loc -> Loc seq) (locations: Loc seq) (start: Loc) (target: Loc) =
    let prev (path: int * Loc) = snd path
    let dist (path: int * Loc) = fst path
    let remove (location: Loc) (source: Loc list) = source |> List.filter (fun l -> not (l = location))
    let total = locations |> Seq.length

    let next remaining ls =
        remaining
        |> Seq.filter (fun location -> ls |> Map.containsKey location)
        |> Seq.minBy (fun location -> ls |> Map.find location |> fst)

    let backtrace target visited =
        Seq.unfold (fun location -> 
            let nextStep = visited |> Map.find location |> prev
            if nextStep = location then 
                None
            else
                Some (nextStep, nextStep)) target
        |> Seq.append (Seq.singleton target)
        |> Seq.rev
        |> Seq.toList

    let rec loop remaining visited =
        if (visited |> Map.count) % 100 = 0 then printfn "progress = %d / %d" (visited |> Map.count) total

        let curr = next remaining visited

        if curr = target then
            backtrace target visited
        else
            let visited = 
                neighbours curr
                |> Seq.fold (fun visited neighbour -> 
                    match visited |> Map.tryFind neighbour with
                    | None -> 
                        let distance = (visited |> Map.find curr |> dist) + (distance curr neighbour)
                        visited |> Map.add neighbour (distance, curr)
                    | Some (prevDistance, _) -> 
                        let currDistance = (visited |> Map.find curr |> dist) + (distance curr neighbour)
                        if prevDistance < currDistance then
                            visited
                        else
                            visited |> Map.add neighbour (currDistance, curr)) visited
                

            loop (remaining |> remove curr) visited
            
    loop (locations |> Seq.toList) (Map.empty |> Map.add start (0, start))

let dijkstraMutable (distance: Loc -> Loc -> int) (neighbours: Loc -> Loc seq) (locations: Loc seq) (start: Loc) (target: Loc) =

    let locs = locations |> Seq.toArray
    let lookup = Dictionary (locs |> Seq.mapi (fun i loc -> KeyValuePair (loc, i)))
    let dists = locs |> Array.map (fun _ -> Int32.MaxValue)
    let prevs = locs |> Array.map (fun _ -> -1)
    let indexSet = HashSet (seq { 0 .. (locs |> Array.length) - 1 })

    let startIndex = locs |> Array.findIndex (fun loc -> loc = start)
    let targetIndex = locs |> Array.findIndex (fun loc -> loc = target)
    let total = locs.Length
    dists.[startIndex] <- 0
    prevs.[startIndex] <- startIndex

    let next () =
        let nextIndex = indexSet :> IEnumerable<int> |> Seq.minBy (fun index -> dists.[index])
        indexSet.Remove(nextIndex) |> ignore

        nextIndex

    let backtrace (targetIndex: int) =
        Seq.unfold (fun index -> 
            let nextStep = prevs.[index]
            if nextStep = index then 
                None
            else
                Some (nextStep, nextStep)) targetIndex
        |> Seq.append (Seq.singleton targetIndex)
        |> Seq.rev
        |> Seq.map (fun index -> locs.[index])
        |> Seq.toList

    let rec loop () =
        if (indexSet.Count) % 100 = 0 then printfn "progress = %d / %d" (total - indexSet.Count) total

        let curr = next ()

        if curr = targetIndex then
            backtrace targetIndex
        else
            for neighbour in neighbours locs.[curr] do
                let neighbourIndex = lookup.[neighbour]
                match dists.[neighbourIndex] with
                | Int32.MaxValue -> 
                    let distance = (dists.[curr]) + (distance locs.[curr] neighbour)
                    dists.[neighbourIndex] <- distance
                    prevs.[neighbourIndex] <- curr
                    ()
                | prevDistance ->
                    let distance = (dists.[curr]) + (distance locs.[curr] neighbour)
                    if prevDistance > distance then
                        dists.[neighbourIndex] <- distance
                        prevs.[neighbourIndex] <- curr
                        ()
                
            loop ()

    loop ()

[<EntryPoint>]
let main _ =

    let map =
        File.ReadAllLines "Input.txt"
        |> Seq.mapi (fun x line ->
            line |> Seq.mapi (fun y risk -> create x y,  (int risk) - (int '0')))
        |> Seq.collect id
        |> Seq.map (fun (loc, risk) -> KeyValuePair (loc, risk))
        |> Dictionary
        
    let risk (map: Dictionary<Loc, int>) path =
        path
        |> Seq.skip 1
        |> Seq.sumBy (fun pos -> map.[pos])

    let locations (map: Dictionary<Loc, int>) = map :> IEnumerable<_> |> Seq.map (fun loc -> loc.Key)
    let distance (map: Dictionary<Loc, int>) _ location = map.[location]
    let neighbours (map: Dictionary<Loc, int>) location = 
        seq { shiftX location 1; shiftX location -1; shiftY location 1; shiftY location -1 }
        |> Seq.choose (fun candidate ->
            match map.TryGetValue(candidate) with
            | (true, _) -> Some candidate
            | (false, _) -> None)
            
    let start = create 0 0
    let target = create 99 99

    let path = dijkstraMutable (distance map) (neighbours map) (locations map) start target

    printfn ""
    printfn "least risky path has risk = %d" (risk map path)
    printfn ""

    let extendedMap =
        locations map
        |> Seq.collect (fun loc ->
            [0..4] 
            |> Seq.collect (fun i ->
                [0..4]
                |> Seq.map (fun j -> 
                    let location = create ((getX loc) + i * 100) ((getY loc) + j * 100)
                    let risk = (map.[loc] + i + j)
                    let risk = if risk > 9 then risk - 9 else risk

                    KeyValuePair (location, risk))))
        |> Dictionary

    let start = create 0 0
    let target = create 499 499

    let path = dijkstraMutable (distance extendedMap) (neighbours extendedMap) (locations extendedMap) start target
    
    printfn ""
    printfn "least risky extended path has risk = %d" (risk extendedMap path)
    printfn ""

    0