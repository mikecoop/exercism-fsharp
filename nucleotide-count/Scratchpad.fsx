
let empty = [ ('A', 0); ('C', 0); ('G', 0); ('T', 0) ] |> Map.ofList

empty.Add('A', empty.['A'] + 1)