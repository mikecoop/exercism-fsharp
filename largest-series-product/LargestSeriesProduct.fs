module LargestSeriesProduct

let largestProduct (input:string) seriesLength =
    if seriesLength = 0 then
        Some 1
    elif seriesLength > input.Length || seriesLength < 0 then
        None
    elif (String.forall System.Char.IsDigit input) = false then
        None
    else
        input.ToCharArray()
        |> List.ofArray
        |> List.map (fun c -> c.ToString())
        |> List.map System.Int32.Parse
        |> List.windowed seriesLength
        |> List.map (fun list -> List.fold (*) 1 list)
        |> List.max
        |> Some