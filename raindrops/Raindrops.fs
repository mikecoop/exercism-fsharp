module Raindrops

let convert (number: int): string =
    let multiples = [(3,"Pling"); (5,"Plang"); (7,"Plong")]
    let isMultipleOf (multiple, _) = number % multiple = 0
    let filterEmpty s = if s = "" then string number else s
    multiples
    |> List.filter isMultipleOf
    |> List.map snd
    |> String.concat ""
    |> filterEmpty
