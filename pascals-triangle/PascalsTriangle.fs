module PascalsTriangle

let nextRow (row:int list) =
    [ 0 ] @ row @ [ 0 ]
    |> List.pairwise
    |> List.map (fun (x, y) -> x + y)

let rows numberOfRows : int list list =
    let rec addNextRow (numRows:int) (list:int list list) =
        match List.length list with
        | x when x = numRows -> list
        | _ -> addNextRow numRows ((nextRow list.Head) :: list)
    match numberOfRows with
    | 0 -> [ ]
    | _ -> addNextRow numberOfRows [ [ 1 ] ] |> List.rev