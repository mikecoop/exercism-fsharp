module Proverb

let recite (input: string list): string list =
    if input = [ ] then
        [ ]
    else
        (input
         |> List.pairwise
         |> List.map (function
             | (first, second) -> sprintf "For want of a %s the %s was lost." first second))
         @ [ sprintf "And all for the want of a %s." (input |> List.head) ]