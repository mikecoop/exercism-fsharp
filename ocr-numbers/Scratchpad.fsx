
let getNumberAtIndex (input:string list) index =
    let startIndex, endIndex = (index * 3, (index * 3) + 2)
    input |> List.map (fun s -> s.[startIndex..endIndex])

let matchNumber input =
    let numbers =
        [ ('0', [ " _ "; "| |"; "|_|"; "   " ])
          ('1', [ "   "; "  |"; "  |"; "   " ])
          ('2', [ " _ "; " _|"; "|_ "; "   " ])
          ('3', [ " _ "; " _|"; " _|"; "   " ])
          ('4', [ "   "; "|_|"; "  |"; "   " ])
          ('5', [ " _ "; "|_ "; " _|"; "   " ])
          ('6', [ " _ "; "|_ "; "|_|"; "   " ])
          ('7', [ " _ "; "  |"; "  |"; "   " ])
          ('8', [ " _ "; "|_|"; "|_|"; "   " ])
          ('9', [ " _ "; "|_|"; " _|"; "   " ]) ]
    let number = numbers |> List.tryFind(fun (_, list) -> list = input)
    match number with
    | None -> '?'
    | Some (s, _) -> s
let getNumberAndMatch input = getNumberAtIndex input >> matchNumber

let getNumbers (input:string list) = 
    [ 0 .. (input.[0].Length / 3) - 1]
    |> List.map (getNumberAndMatch input)
    |> List.toArray
    |> System.String.Concat

let convert (input:string list) =
    if (input |> List.length) % 4 <> 0 then
        None
    elif input |> List.forall (fun s -> s.Length % 3 <> 0) then
        None
    else
        let result =
            input
            |> List.chunkBySize 4
            |> List.map getNumbers
            |> String.concat ","
        Some (result)

[["    _  _ "; "  | _| _|"; "  ||_  _|"; "         "];
 ["    _  _ "; "|_||_ |_ "; "  | _||_|"; "         "];
 [" _  _  _ "; "  ||_||_|"; "  ||_| _|"; "         "]]
|> List.map getNumbers

[ "    _  _ ";
  "  | _| _|";
  "  ||_  _|";
  "         ";
  "    _  _ ";
  "|_||_ |_ ";
  "  | _||_|";
  "         ";
  " _  _  _ ";
  "  ||_||_|";
  "  ||_| _|";
  "         " ]
|> convert