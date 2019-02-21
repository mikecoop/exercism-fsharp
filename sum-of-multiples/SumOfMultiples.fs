module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let isMultipleOf number multiple = number % multiple = 0
    let isMultipleOfAny number = numbers |> List.filter ((<) 0) |> List.exists (isMultipleOf number)
    [1..upperBound-1]
    |> List.filter isMultipleOfAny
    |> List.sum