module SimpleLinkedList

type LinkedList =
    | Nil
    | Item of Value:int * Next:LinkedList

let nil = Nil

let create x n =
    Item (x, n)

let isNil x =
    match x with
    | Nil -> true
    | Item _ -> false

let next x =
    match x with
    | Nil -> Nil
    | Item (_, n) -> n

let datum x =
    match x with
    | Nil -> 0
    | Item (i, _) -> i

let toList (x:LinkedList) =
    let rec buildList list linked =
        match linked with
        | Nil ->
            list
        | Item (item, next) ->
            let newList = list @ [ item ]
            buildList newList next
    buildList List.empty x

let fromList (xs:int List) =
    List.foldBack create xs Nil

let reverse x =
    x |> toList |> List.rev |> fromList