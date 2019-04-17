module WordSearch

let rows matrix = List.length matrix
let columns matrix = String.length (List.head matrix)

let groupItem item =
    (item |> List.map fst, item |> List.map snd |> List.toArray |> System.String)

let leftDiagonals matrix =
    let r = rows matrix
    let c = columns matrix
    [ for slice in 0 .. r + c - 2 do
        let z1 = if slice < c then 0 else slice - c + 1
        let z2 = if slice < r then 0 else slice - r + 1
        yield [ for j in slice - z2 .. -1 .. z1 do
                yield ((slice - j + 1, j + 1), matrix.[j].[slice - j])]]
    |> List.map groupItem

let rightDiagonals (matrix:string list) =
    let diagonals =
        matrix
        |> List.map (Seq.rev >> Seq.toArray >> System.String)
        |> leftDiagonals

    let c = columns matrix
    diagonals
    |> List.map (fun d ->
        d |> fst |> List.map (fun (col, row) -> c - (col - 1), row),
        d |> snd)

let findInRow (word:string) (row:string) =
    match row.IndexOf (word) with
    | -1 ->
        let wordReversed = System.String (Seq.rev word |> Seq.toArray)
        match row.IndexOf (wordReversed) with
        | -1 -> None
        | index -> Some (index + word.Length - 1, index)
    | index -> Some (index, index + word.Length - 1)

let findInRows (word:string) (rows:string list) =
    let matches =
        rows
        |> List.indexed
        |> List.choose
            (fun (row, rowStr) ->
                let columns = rowStr |> findInRow word
                match columns with
                | None -> None
                | Some (y1, y2) -> Some ((y1 + 1, row + 1), (y2 + 1, row + 1)))
    match matches with
    | [ ] -> None
    | first :: _ -> Some first

let findInColumns (word:string) (rows:string list) =
    let columns =
        rows
        |> List.map (fun str -> str.ToCharArray() |> List.ofArray)
        |> List.transpose
        |> List.map (List.toArray >> System.String)
    match (findInRows word columns) with
    | Some ((y1, x1), (y2, x2)) -> Some ((x1, y1), (x2, y2))
    | None -> None

let findInDiagonals (word:string) (diagonals:(((int * int) list) * string) list) =
    let matches =
        diagonals
        |> List.choose
            (fun (indexes, str) ->
                let columns = findInRow word str
                match columns with
                | None -> None
                | Some (y1, y2) ->  Some (indexes.[y1], indexes.[y2]))
    match matches with
    | [ ] -> None
    | first :: _ -> Some first

let findMatch grid word =
    match (grid |> findInRows word) with
    | Some location -> Some location
    | None ->
        match (grid |> findInColumns word) with
        | Some location -> Some location
        | None ->
            match (grid |> rightDiagonals |> findInDiagonals word) with
            | Some location -> Some location
            | None ->
                match (grid |> leftDiagonals |> findInDiagonals word) with
                | Some location -> Some location
                | None -> None

let search grid (wordsToSearchFor:string list) =
    wordsToSearchFor
    |> List.map (fun word -> word, findMatch grid word)
    |> Map.ofList