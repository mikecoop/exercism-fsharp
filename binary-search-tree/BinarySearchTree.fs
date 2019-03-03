module BinarySearchTree

type BinaryTree = { Value: int; Left: BinaryTree option; Right: BinaryTree option }

let left node = node.Left

let right node = node.Right

let data node = node.Value

let rec insert (tree:BinaryTree option) (value:int) =
    match tree with
    | Some t ->
        if value <= t.Value then
            { t with Left = Some (insert t.Left value) }
        else
            { t with Right = Some (insert t.Right value) }
    | None ->
        { Value = value; Left = None; Right = None }

let create items =
    let tree = items |> List.fold (fun acc value -> Some (insert acc value)) None
    match tree with
    | Some t -> t
    | None -> failwith "No values in tree"

let sortedData (node:BinaryTree) =
    let rec traverse tree =
        match tree with
        | None -> [ ]
        | Some t -> (traverse t.Left) @ [ t.Value ] @ (traverse t.Right)
    traverse (Some node)