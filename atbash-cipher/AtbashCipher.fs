module AtbashCipher

let alphabet = [ 'a'..'z' ]

let pairMap = List.zip alphabet (List.rev alphabet) |> Map.ofList

let swap (str:string) =
    str.ToLower()
    |> String.map (fun c ->
        match pairMap.TryGetValue c with
        | true, swapped -> swapped
        | false, _ -> c)

let filterAlphaNumeric str = String.filter (System.Char.IsLetterOrDigit) str

let chunkByFive (str:string) =
    Seq.chunkBySize 5 str
    |> Seq.map (fun arr -> System.String(arr))
    |> String.concat " "

let encode = swap >> filterAlphaNumeric >> chunkByFive

let decode = filterAlphaNumeric >> swap