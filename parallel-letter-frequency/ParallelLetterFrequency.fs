module ParallelLetterFrequency

let letterFrequency (text:string) =
    text
    |> Seq.map System.Char.ToLower
    |> Seq.filter System.Char.IsLetter
    |> Seq.countBy id
    |> Map.ofSeq

let foldMaps (map:Map<char, int>) =
    map
    |> Map.fold (fun acc key frequency ->
        match Map.tryFind key acc with
        | Some accFrequency -> acc.Add(key, frequency + accFrequency)
        | None -> acc.Add(key, frequency))

let frequency (texts:string list) =
    match texts with
    | [ ] -> Map.empty
    | _ ->
        [ for t in texts ->
            async { return letterFrequency t }]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold foldMaps Map.empty