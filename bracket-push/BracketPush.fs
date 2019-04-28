module BracketPush

let bracketPairs =
    [ ('[', ']')
      ('{', '}')
      ('(', ')') ]

let pushPop list char =
    match bracketPairs |> List.tryFind (fun (first, _) -> first = char) with
    | Some pair ->
        fst pair :: list
    | None ->
        match bracketPairs |> List.tryFind (fun (_, second) -> second = char) with
        | None -> list
        | Some pair ->
            match list with
            | next :: rest ->
                if fst pair = next then
                    rest
                else
                    snd pair :: list
            | _ -> list

let isPaired (input:string) =
    let result = input.ToCharArray() |> List.ofArray |> Seq.fold pushPop [ ]
    List.isEmpty result