module CustomSet

type Set<'a> =
    { Elements: 'a list }

let addElement element set =
    if set.Elements |> List.contains element then
        set
    else
        let newElements = element :: set.Elements |> List.sort
        { Elements = newElements }

let empty = { Elements = List.empty }

let isEmpty set =
    Seq.isEmpty set.Elements

let size set =
    List.length set.Elements

let fromList list =
    { Elements = list |> List.distinct |> List.sort }

let toList set =
    set.Elements

let contains value set =
    set.Elements |> Seq.contains value

let insert value set =
    addElement value set

let union left right =
    fromList (left.Elements @ right.Elements)

let intersection left right =
    match left.Elements, right.Elements with
    | [ ], _ -> empty
    | _, [ ] -> empty
    | leftVals, rightVals ->
        leftVals
        |> List.filter (fun v -> rightVals |> List.contains v)
        |> fromList

let difference left right =
    match left.Elements, right.Elements with
    | [ ], _ -> empty
    | leftVals, [ ] -> fromList leftVals
    | leftVals, rightVals ->
        leftVals
        |> List.except rightVals
        |> fromList

let isSubsetOf left right =
    match left.Elements, right.Elements with
    | [ ], _ -> true
    | _, [ ] -> false
    | leftVals, rightValues ->
        leftVals
        |> List.map (fun v -> rightValues |> List.contains v)
        |> List.reduce (&&)

let isDisjointFrom left right =
    match left.Elements, right.Elements with
    | [ ], _ -> true
    | _, [ ] -> true
    | leftVals, rightValues ->
        not (leftVals
        |> List.exists (fun v -> rightValues |> List.contains v))

let isEqualTo left right =
    List.length left.Elements = List.length right.Elements &&
    isSubsetOf left right &&
    isSubsetOf right left