module IsbnVerifier

let (<|>) f g = (fun x -> f x || g x)

let isValid isbn =
    let getDigits =
        let isX (c:char) = System.Char.ToUpper(c) = 'X'
        Seq.filter (System.Char.IsDigit <|> isX)
        >> Seq.indexed
        >> Seq.choose (function
            | 9, 'X' | 9, 'x' -> Some 10
            | _, 'X' | _, 'x' -> None
            | _, v -> Some (System.Int32.Parse (string v)))
        >> List.ofSeq
    let digits = getDigits isbn
    List.length digits = 10 &&
    (List.zip digits [ 10 .. -1 .. 1 ]
    |> List.sumBy (fun (x, y) -> x * y)) % 11 = 0