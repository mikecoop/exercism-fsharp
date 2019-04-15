module Transpose

let transposeList (input:char list list) : char list list =
    let maxLength list = list |> List.map (List.length) |> List.max
    let maxAfterRow row = input |> List.skip (row + 1) |> maxLength
    [ for column in 0 .. (input |> maxLength) - 1 ->
        input
        |> List.indexed
        |> List.choose
            (fun (row, chars) ->
                match chars |> List.tryItem column with
                | None _ ->
                    if (row < List.length input - 1) &&
                        (List.length chars) <= (maxAfterRow row) then
                        Some ' '
                    else
                        None
                | c -> c ) ]

let transpose (input:string list) =
    if List.isEmpty input then
        List.empty
    else
        input
        |> List.map (fun str -> str.ToCharArray() |> List.ofArray)
        |> transposeList
        |> List.map (List.toArray >> System.String)

[ "The longest line.";
          "A long line.";
          "A longer line.";
          "A line." ] |> transpose