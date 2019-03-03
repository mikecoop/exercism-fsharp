module DifferenceOfSquares

let squareOfSum (number: int): int =
    let sum =
        [ 1 .. number ]
        |> List.reduce (+)
    pown sum 2

let sumOfSquares (number: int): int =
    [ 1 .. number ]
    |> List.map (fun x -> x * x)
    |> List.reduce (+)

let differenceOfSquares (number: int): int =
    (squareOfSum number) - (sumOfSquares number)