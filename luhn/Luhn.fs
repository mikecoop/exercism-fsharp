module Luhn

open System

let valid number =
    let isDigitOrWhiteSpace c = System.Char.IsDigit(c) || System.Char.IsWhiteSpace(c)
    let allDigitsOrWhitespace str = String.forall isDigitOrWhiteSpace str

    if not (allDigitsOrWhitespace number) then
        false
    else
        let spacesRemoved = String.filter System.Char.IsDigit number
        if String.length spacesRemoved <= 1 then
            false
        else
            let sum =
                spacesRemoved
                |> Seq.filter System.Char.IsDigit
                |> Seq.map (fun c -> c.ToString() |> Int32.Parse)
                |> Seq.rev
                |> Seq.mapi (fun index value -> if index % 2 = 1 then value * 2 else value)
                |> Seq.map (fun value -> if value > 9 then value - 9 else value )
                |> Seq.sum
            sum % 10 = 0