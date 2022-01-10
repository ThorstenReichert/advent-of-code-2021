open System
open System.IO

type Version = Version of int32
type TypeId = TypeId of int32
type LiteralValue = LiteralValue of int64
type SubPacketsLength = SubPacketsLength of int
type SubPacketsCount = SubPacketsCount of int

type Packet = 
| LiteralPacket of Version * TypeId * LiteralValue
| OperatorPacket of Version * TypeId * Packet list

let (|Bits|) hex = 
    match hex with
    | '0' -> [0;0;0;0]
    | '1' -> [0;0;0;1]
    | '2' -> [0;0;1;0]
    | '3' -> [0;0;1;1]
    | '4' -> [0;1;0;0]
    | '5' -> [0;1;0;1]
    | '6' -> [0;1;1;0]
    | '7' -> [0;1;1;1]
    | '8' -> [1;0;0;0]
    | '9' -> [1;0;0;1]
    | 'A' -> [1;0;1;0]
    | 'B' -> [1;0;1;1]
    | 'C' -> [1;1;0;0]
    | 'D' -> [1;1;0;1]
    | 'E' -> [1;1;1;0]
    | 'F' -> [1;1;1;1]
    | _ -> failwith "unreconized hex char"

let toInt32 (bits: int32 seq) =
    Convert.ToInt32 (bits |> Seq.map (fun b -> b.ToString()) |> String.Concat, 2)
let toInt64 (bits: int32 seq) =
    Convert.ToInt64 (bits |> Seq.map (fun b -> b.ToString()) |> String.Concat, 2)

let split length source =
    (source |> List.take length, source |> List.skip length)

let (|TransmissionEnd|_|) bits =
    if bits |> List.forall (fun bit -> bit = 0) then Some ()
    else None

let (|Header|_|) (bits: int32 list) =
    match bits with 
    | v1::v2::v3::t1::t2::t3::tail -> Some (toInt32 [|v1;v2;v3|] |> Version, toInt32 [|t1;t2;t3|] |> TypeId, tail)
    | _ -> None

let (|Literal|_|) bits =
    let rec loop acc bits =
        match bits with
        | 1::l1::l2::l3::l4::tail -> loop (l4::l3::l2::l1::acc) tail
        | 0::l1::l2::l3::l4::tail -> (l4::l3::l2::l1::acc, tail)
        | _ -> failwith "invalid literal"

    match bits with
    | Header (version, typeId & TypeId 4, remaining) -> 
        let (literalBits, remaining) = loop [] remaining

        (version, typeId, literalBits |> Seq.rev |> toInt64 |> LiteralValue, remaining) |> Some

    | _ -> None

let (|OperatorWithLength|_|) bits =
    match bits with
    | Header (version, typeId, 0::remaining) when typeId <> TypeId 4 -> 
        let (length, remaining) = remaining |> split 15
        
        (version, typeId, SubPacketsLength (toInt32 length), remaining) |> Some

    | _ -> None

let (|OperatorWithCount|_|) bits =
    match bits with
    | Header (version, typeId, 1::remaining) when typeId <> TypeId 4 ->
        let (count, remaining) = remaining |> split 11

        (version, typeId, SubPacketsCount (toInt32 count), remaining) |> Some

    | _ -> None

let rec parsePackets bits =
    let rec nextPacket bits = 
        match bits with
        | TransmissionEnd _ -> 
            printfn "End"
            None

        | Literal (Version v & version, typeId, LiteralValue l & value, remaining) -> 
            printfn "Literal [version = %d; value = %d]" v l
            let packet = LiteralPacket (version, typeId, value)
            
            Some (packet, remaining)

        | OperatorWithLength (Version v & version, typeId, SubPacketsLength length, remaining) -> 
            printfn "Operator with length [version = %d; length = %d]" v length
            let (subs, remaining) = remaining |> split length
            let packet = OperatorPacket (version, typeId, parsePackets subs)
            
            Some (packet, remaining)

        | OperatorWithCount (Version v & version, typeId, SubPacketsCount count, remaining) ->
            printfn "Operator with count [version = %d; count = %d]" v count
            let (subs, remaining) = 
                [0..count-1]
                |> List.fold (fun (acc, rs) _ -> 
                    let (next, rs) = (nextPacket rs).Value
                    (next :: acc, rs)) ([], remaining)
            let packet = OperatorPacket (version, typeId, subs |> List.rev)

            Some (packet, remaining)

        | _ -> failwith "unrecognized packet"

    let rec loop acc remaining =
        match nextPacket remaining with
        | Some (packet, remaining) -> loop (packet :: acc) remaining
        | None -> acc
        
    loop [] bits |> List.rev

let rec interpret =
    function
    | LiteralPacket (_, _, LiteralValue value) -> value
    | OperatorPacket (_, TypeId 0, operands) -> operands |> List.map interpret |> List.sum
    | OperatorPacket (_, TypeId 1, operands) -> operands |> List.map interpret |> List.reduce (*)
    | OperatorPacket (_, TypeId 2, operands) -> operands |> List.map interpret |> List.min
    | OperatorPacket (_, TypeId 3, operands) -> operands |> List.map interpret |> List.max
    | OperatorPacket (_, TypeId 5, operands) -> operands |> List.map interpret |> (fun ops -> if (ops |> List.item 0) > (ops |> List.item 1) then 1L else 0L)
    | OperatorPacket (_, TypeId 6, operands) -> operands |> List.map interpret |> (fun ops -> if (ops |> List.item 0) < (ops |> List.item 1) then 1L else 0L)
    | OperatorPacket (_, TypeId 7, operands) -> operands |> List.map interpret |> (fun ops -> if (ops |> List.item 0) = (ops |> List.item 1) then 1L else 0L)
    | _ -> failwith "unrecognized operation"

let rec versionSum packets =
    packets
    |> List.fold (fun sum packet ->
        match packet with
        | LiteralPacket (Version version, _, _) -> sum + version
        | OperatorPacket (Version version, _, subPackets) ->
            sum + version + (versionSum subPackets)) 0

[<EntryPoint>]
let main _ =
    let bits =
        File.ReadAllText "Input.txt"
        |> Seq.collect (fun (Bits bs) -> bs)
        |> Seq.toList
    
    let transmission = parsePackets bits

    printfn ""
    printfn "version sum = %d" (versionSum transmission)
    printfn "result      = %d" (transmission |> List.exactlyOne |> interpret)

    0