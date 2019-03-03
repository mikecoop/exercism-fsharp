module RnaTranscription

let toRna (dna: string): string =
    dna
    |> String.collect (function
        | 'C' -> "G"
        | 'G' -> "C"
        | 'T' -> "A"
        | 'A' -> "U"
        | _ -> "")