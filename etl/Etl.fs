module Etl

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
    [ for (score, letters) in (scoresWithLetters |> Map.toList) do
        for letter in letters do
            yield (System.Char.ToLower(letter), score)]
    |> Map.ofList