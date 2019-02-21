module OcrNumbers

let convert (input:string list) =
    if List.length input <> 4 then
        None
    elif (input |> List.forall (fun s -> s.Length % 3 <> 0)) then
        None
    else
        let getNumberText (input:string list) index =
            let startIndex, endIndex = (index * 3, (index * 3) + 2)
            input |> List.map (fun s -> s.[startIndex..endIndex])
        let getNumberMatch input =
            let space3 = "   "
            let numbers =
                [ ('0', [ " _ "; "| |"; "|_|"; space3 ])
                  ('1', [ space3; "  |"; "  |"; space3 ]) ]
            let number = numbers |> List.tryFind(fun (_, list) -> list = input)
            match number with
            | None -> '?'
            | Some (s, _) -> s
        let getNumberTextMatch = getNumberText input >> getNumberMatch
        let result = 
            [ 0 .. (input.[0].Length / 3) - 1]
            |> List.map getNumberTextMatch
            |> List.toArray
        Some (System.String.Concat(result))