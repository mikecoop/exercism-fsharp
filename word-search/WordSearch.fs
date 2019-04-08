module WordSearch

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

let findMatch grid word =
    match (grid |> findInRows word) with
    | Some location ->
        Some location
    | None ->
        match (grid |> findInColumns word) with
        | Some location -> Some location
        | None -> None

let search grid (wordsToSearchFor:string list) =
    wordsToSearchFor
    |> List.map (fun word -> word, findMatch grid word)
    |> Map.ofList