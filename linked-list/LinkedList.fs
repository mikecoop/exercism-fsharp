module LinkedList
open System.Collections.Generic
open System.Collections.Generic

type Node = {
    mutable Previous:Node option
    Value:int
    mutable Next:Node option
}

type LinkedList = {
    mutable First:Node option
    mutable Last:Node option
}

let node value = { Previous = None; Value = value; Next = None }
let nodeWithPrev value (prev:Node) = { node value with Previous = Some (prev) }
let nodeWithNext value (next:Node) = { node value with Next = Some (next) }

let mkLinkedList () = { First = None; Last = None }

let addToEmpty node linkedList =
    linkedList.First <- Some node
    linkedList.Last <- Some node

let pop linkedList =
    match linkedList.Last with
    | None -> failwith "List is empty"
    | Some last ->
        linkedList.Last <- last.Previous
        last.Value

let shift linkedList =
    match linkedList.First with
    | None -> failwith "List is empty"
    | Some first ->
        linkedList.First <- first.Next
        first.Value

let push newValue linkedList =
    match linkedList.Last with
    | None ->
        linkedList |> addToEmpty (node newValue)
    | Some last ->
        let newNode = Some (nodeWithPrev newValue last)
        last.Next <- newNode
        linkedList.Last <- newNode

let unshift newValue linkedList =
    match linkedList.First with
    | None ->
        linkedList |> addToEmpty (node newValue)
    | Some first ->
        let newNode = Some (nodeWithNext newValue first)
        first.Previous <- newNode
        linkedList.First <- newNode