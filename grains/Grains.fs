module Grains

let squareUL (n: int) = pown 2UL (n-1)

let square (n: int): Result<uint64,string> =
    match n with
    | n when n < 1 || n > 64 -> Error "square must be between 1 and 64"
    | n -> Ok (squareUL n)

let total: Result<uint64,string> =
    Ok ([1..64] |> List.sumBy squareUL)