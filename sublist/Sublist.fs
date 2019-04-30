module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal

let rec isSublist list sublist =
    if List.isEmpty sublist then
        true
    elif List.length sublist > List.length list then
        false
    elif List.head sublist = List.head list then
        let check =
            sublist
            |> List.mapi (fun i x -> x, list.[i])
            |> List.map (fun (x, y) -> x = y)
            |> List.reduce (&&)
        if check then
            true
        else
            isSublist (List.tail list) sublist
    else
        isSublist (List.tail list) sublist

let sublist xs ys =
    match xs |> isSublist ys, ys |> isSublist xs with
    | true, true -> Equal
    | true, false -> Sublist
    | false, true -> Superlist
    | false, false -> Unequal