module Isogram

let isIsogram (str:string) =
    let onlyLetters = str.ToUpper() |> Seq.filter System.Char.IsLetter |> Seq.toList
    onlyLetters.Length = (onlyLetters |> Set.ofList |> Set.count)