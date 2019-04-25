module RailFenceCipher

let pattern n = [ 1 .. n ] @ [ (n - 1) .. -1 .. 2 ]

let repeatedPattern rails message =
    let pattern = pattern rails
    List.replicate ((Seq.length message / List.length pattern) + 1) pattern
    |> List.collect id
    |> List.take (Seq.length message)

let encode (rails:int) (message:string) : string =
    let repeated = repeatedPattern rails message
    let pairs = Seq.zip repeated message

    seq { for n in 1 .. rails do
            yield! (pairs
            |> Seq.where (fun (rail, _) -> rail = n)
            |> Seq.map snd) }
    |> Seq.toArray
    |> System.String

let rec splitList acc chunkSizes list =
    match chunkSizes with
    | [ ] -> acc |> List.rev
    | size :: sizes ->
        let chunk, rest = List.splitAt size list
        splitList (chunk :: acc) sizes rest

let decode (rails:int) (message:string) =
    let repeated = repeatedPattern rails message
    let chunks =
        splitList [ ] (repeated |> List.countBy id |> List.map snd) (message |> Seq.toList)
    chunks
    |> List.map (List.indexed >> Map.ofList)
    |> List.mapi (fun index chunk -> index + 1, chunk)
    |> Map.ofList

decode 3 "TEITELHDVLSNHDTISEIIEA"