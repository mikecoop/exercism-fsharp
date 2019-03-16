module DotDsl

type GraphItem =
    | Node of Node
    | Edge of Edge
    | Attribute of Attribute
and Node = { Key: string; Attributes: Attribute list }
and Edge = { Left: string; Right: string; Attributes: Attribute list }
and Attribute = string * string

type Graph = { Children: GraphItem list }

let graph children : Graph =
    { Children = children }

let attr key value =
    Attribute (key, value)

let node key attrs =
    Node { Key = key; Attributes = attrs }

let edge left right attrs =
    Edge { Left = left; Right = right; Attributes = attrs }

let sortKey = function
    | Node n -> n.Key
    | Edge e -> e.Left
    | Attribute a -> fst a

let chooseGraphItems fChoose graph =
    graph.Children
    |> List.choose fChoose
    |> List.sortBy sortKey

let attrs =
    chooseGraphItems (function
    | Attribute a -> Some (Attribute a)
    | _ -> None)

let nodes =
    chooseGraphItems (function
    | Node n -> Some (Node n)
    | _ -> None)

let edges =
    chooseGraphItems (function
    | Edge e -> Some (Edge e)
    | _ -> None)