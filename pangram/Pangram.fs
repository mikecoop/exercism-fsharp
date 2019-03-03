module Pangram

let isPangram (input: string): bool =
    let inputSet = input.ToUpper() |> Set.ofSeq
    let allLettersSet = [ 'A' .. 'Z' ] |> Set.ofList
    (Set.intersect inputSet allLettersSet) = allLettersSet