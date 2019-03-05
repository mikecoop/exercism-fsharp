module ScrabbleScore

let score word =
    let scores =
        [ ( [ 'A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T'], 1)
          ( [ 'D'; 'G' ], 2 )
          ( [ 'B'; 'C'; 'M'; 'P' ], 3 )
          ( [ 'F'; 'H'; 'V'; 'W'; 'Y' ], 4 )
          ( [ 'K' ], 5 )
          ( [ 'J'; 'X' ], 8 )
          ( [ 'Q'; 'Z' ], 10 ) ]
    let letterScore l =
        scores 
        |> List.find (fun (letters, _) -> letters |> List.contains (System.Char.ToUpper l))
        |> snd
    word
    |> Seq.map letterScore
    |> Seq.sum