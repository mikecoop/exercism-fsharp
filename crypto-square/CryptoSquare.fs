module CryptoSquare

open System

let ciphertext input =
    let normalize input =
        input
        |> String.filter Char.IsLetterOrDigit
        |> String.map Char.ToLower

    let applyCipher normalized =
        let columns = normalized |> String.length |> float |> sqrt |> ceil |> int
        normalized
        |> Seq.chunkBySize columns
        |> Seq.transpose
        |> Seq.map (Seq.toArray >> System.String)

    let appendSpaces desiredLength str =
        if String.length str < desiredLength then
            str + String.replicate (desiredLength - String.length str) " "
        else
            str

    if input = "" then
        ""
    else
        let words = normalize input |> applyCipher
        words
        |> Seq.map (appendSpaces (Seq.head words).Length)
        |> String.concat " "