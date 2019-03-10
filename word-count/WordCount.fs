module WordCount

open System

let countWords (phrase:string) =
    let isValidChar c = System.Char.IsLetterOrDigit c || c = ',' || c = ' ' || c = '\''
    let filtered = String.filter isValidChar phrase
    filtered.ToLower().Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun w -> w.TrimStart('\'').TrimEnd('\''))
    |> Array.countBy id
    |> Map.ofArray