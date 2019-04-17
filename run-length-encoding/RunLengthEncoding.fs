module RunLengthEncoding

open System
open System.Text.RegularExpressions

let encode (input:string) : string =
    let folder char counts =
        match counts with
        | [ ] -> [ (1, char) ]
        | (count, lastChar) :: rest ->
            if char = lastChar then
                (count + 1, lastChar) :: rest
            else
                (1, char) :: counts

    let countToString (count, char) =
        if count = 1 then
            char.ToString()
        else
            sprintf "%i%c" count char

    Seq.foldBack folder input [ ]
    |> List.map countToString
    |> String.concat ""

let decode (input:string) =
    let regex = Regex ("(\d+)?(\D)")
    let matches = regex.Matches (input) |> Seq.cast<Match>
    let countPairs =
        matches
        |> Seq.toList
        |> List.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
    let pairToString (count, char) =
        match count with
        | "" -> char
        | _ -> String.replicate (Int32.Parse count) char

    countPairs
    |> List.map pairToString
    |> String.concat ""