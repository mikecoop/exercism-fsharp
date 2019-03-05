module ProteinTranslation

let proteins (rna:string) =
    let getProtein = function
    | "AUG" -> 
        "Methionine"
    | "UUU" | "UUC" -> 
        "Phenylalanine"
    | "UUA" | "UUG" ->
        "Leucine"
    | "UCU" | "UCC" | "UCA" | "UCG" ->
        "Serine"
    | "UAU" | "UAC" ->
        "Tyrosine"
    | "UGU" | "UGC" ->
        "Cysteine"
    | "UGG" -> "Tryptophan"
    | "UAA" | "UAG" | "UGA" -> "STOP"
    | _ -> ""
    rna
    |> Seq.chunkBySize 3
    |> Seq.map System.String.Concat
    |> Seq.toList
    |> List.map getProtein
    |> List.takeWhile ((<>) "STOP")