module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, _) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch _ -> true
    | _-> false

let children t = 
    match t with
    | Branch (_, c) -> c
    | _ -> []

let validateRecords records =
    match records with
    | [ ] ->
        failwith "Empty input"
    | x :: _ when x.RecordId <> 0 ->
        failwith "No root node"
    | x :: _ when x.ParentId <> 0 ->
        failwith "Root node has parent"
    | _ :: xs when xs |> List.exists (fun r -> r.RecordId < r.ParentId) ->
        failwith "ParentId should be less than RecordId"
    | _ :: xs when xs |> List.exists (fun r -> r.RecordId = r.ParentId) ->
        failwith "ParentId cannot be the RecordId except for the root node."
    | records when (records |> List.map (fun r -> r.RecordId) |> List.max) > (List.length records - 1) ->
        failwith "Ids are not continuous"
    | _ -> records

let rec makeTree id map =
    match map |> Map.tryFind id with
    | None ->
        Leaf id
    | Some list ->
        Branch (id, list |> List.map (fun r -> makeTree r.RecordId map))

let buildTree records = 
    records
    |> List.sortBy (fun r -> r.RecordId)
    |> validateRecords
    |> List.tail
    |> List.groupBy (fun r -> r.ParentId)
    |> Map.ofList
    |> makeTree 0