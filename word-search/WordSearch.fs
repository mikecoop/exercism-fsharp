module WordSearch

let tryFindIndex (word:string) (row:string) =
    match row.IndexOf (word.[0]) with
    | -1 -> None
    | index ->
        let afterMatch = row.[index..]
        let beforeMatch = row.[..index]
        let wordRev = System.String (Seq.rev word |> Seq.toArray)
        if afterMatch.Length >= word.Length && afterMatch.[.. word.Length - 1] = word then
            Some (index, index + word.Length - 1)
        elif beforeMatch.Length >= word.Length && beforeMatch.[index - (word.Length - 1) ..] = wordRev then
            Some (index, word.Length - 1 - index)
        else
            None

let tryFindInRows (word:string) (rows:string list) =
    let matches =
        rows
        |> List.indexed
        |> List.choose
            (fun (row, rowStr) ->
                let columns = rowStr |> tryFindIndex word
                match columns with
                | None -> None
                | Some (y1, y2) -> Some ((y1 + 1, row + 1), (y2 + 1, row + 1)))
    match matches with
    | [ ] -> None
    | first :: _ -> Some first

let search grid (wordsToSearchFor:string list) =
    wordsToSearchFor
    |> List.map
        (fun word -> word, grid |> tryFindInRows word)
    |> Map.ofList