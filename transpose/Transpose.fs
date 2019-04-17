module Transpose

let maxRowLength matrix =
    matrix |> List.map (List.length) |> List.max

let transposeList (input:char list list) : char list list =
    [ for column in 0 .. (input |> maxRowLength) - 1 ->
        input
        |> List.choose (fun chars ->
                match chars |> List.tryItem column with
                | None _ -> None
                | c -> c ) ]

let maxRowLengthAfterRow rowIndex matrix =
    match matrix |> List.skip (rowIndex + 1) with
    | [ ] -> None
    | list -> Some (list |> maxRowLength)

let normalizeLengths matrix =
    matrix
    |> List.mapi
        (fun rowIndex row ->
            match maxRowLengthAfterRow rowIndex matrix with
            | None -> row
            | Some maxLength ->
                let rowLength = List.length row
                if rowLength < maxLength then
                    let spacesToAdd = maxLength - rowLength
                    row @ (List.replicate spacesToAdd ' ')
                else
                    row)

let transpose (input:string list) =
    if List.isEmpty input then
        List.empty
    else
        input
        |> List.map (fun str -> str.ToCharArray() |> List.ofArray)
        |> normalizeLengths
        |> transposeList
        |> List.map (List.toArray >> System.String)