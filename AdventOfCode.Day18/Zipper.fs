module Zipper

// see http://tomasp.net/blog/tree-zipper-query.aspx/

type Tree<'T> = 
| Node of Tree<'T> * Tree<'T>
| Leaf of 'T

type Path<'T> =
| Top
| Left of Path<'T> * Tree<'T>
| Right of Path<'T> * Tree<'T>

type TreeZipper<'T> =
| TreeZipper of Tree<'T> * Path<'T>

let left = function
    | TreeZipper (Leaf _, _) -> failwith "cannot go left"
    | TreeZipper (Node (l, r), p) -> TreeZipper (l, Left(p, r))

let rec leftLeaf tz =
    match tz with
    | TreeZipper (Node _, _) -> leftLeaf (left tz)
    | TreeZipper (Leaf _, _) -> tz

let right = function
    | TreeZipper (Leaf _, _) -> failwith "canot go right"
    | TreeZipper (Node(l,r), p) -> TreeZipper (r, Right(p, l))

let rec rightLeaf tz =
    match tz with
    | TreeZipper (Node _, _) -> rightLeaf (right tz)
    | TreeZipper (Leaf _, _) -> tz
    
let tryUp = function 
    | TreeZipper (l, Left(p, r))
    | TreeZipper (r, Right(p, l)) -> TreeZipper (Node (l,r), p) |> Some
    | TreeZipper (_, Top) -> None

let up tz =
    match tryUp tz with
    | Some tz -> tz
    | None -> failwith "cannot go up"

let tryNextLeaf tz =
    let rec searchRightParent tz = 
        match tz with
        | TreeZipper (_, Left _) -> Some (up tz)
        | TreeZipper (_, Right _) -> searchRightParent (up tz)
        | TreeZipper (_, Top) -> None

    match tz with
    | TreeZipper (Node _, _) -> failwith "cannot go to next from node"
    | TreeZipper (Leaf _, _) -> 
        searchRightParent tz
        |> Option.map right
        |> Option.map leftLeaf

let nextLeaf tz =
    match tryNextLeaf tz with
    | Some tz -> tz
    | None -> failwith "cannot go to next"

let tryPrevLeaf tz =
    let rec searchLeftParent tz =
        match tz with
        | TreeZipper (_, Left _) -> searchLeftParent (up tz)
        | TreeZipper (_, Right _) -> Some (up tz)
        | TreeZipper (_, Top) -> None

    match tz with
    | TreeZipper (Node _, _) -> failwith "cannot go to prev from node"
    | TreeZipper (Leaf _, _) ->
        searchLeftParent tz
        |> Option.map left
        |> Option.map rightLeaf

let prevLeaf tz =
    match tryPrevLeaf tz with
    | Some tz -> tz 
    | None -> failwith "cannot go to prev"

let rec top = function
    | TreeZipper(_, Top) as t -> t
    | tz -> top (up tz)

let current = function
    | TreeZipper (Leaf x, _) -> x
    | _ -> failwith "cannot get current"

let depth (TreeZipper (_, path)) =
    let rec loop path =
        match path with
        | Top -> 0
        | Left (p, _)
        | Right (p, _) -> 1 + loop p

    loop path

let unit v = TreeZipper(Leaf v, Top)

let zipper tree = TreeZipper (tree, Top)

let bindSub f tz = 
    let rec bindT = function
        | Leaf x -> let (TreeZipper (t, _)) = top (f x) in t
        | Node (l, r) -> Node (bindT l, bindT r)

    let (TreeZipper (current, path)) = tz
    TreeZipper (bindT current, path)

let mapSub f tz = bindSub (f >> unit) tz

let replace replacement (TreeZipper (_, p)) =
    let (TreeZipper (t, _)) = top replacement
    TreeZipper (t, p)
