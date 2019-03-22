module ScaleGenerator

let sharps = [ "A"; "A#"; "B"; "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#" ]

let flats = [ "A"; "Bb"; "B"; "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab" ]

let usesSharps = [ "C"; "G"; "D"; "A"; "E"; "B"; "F#"; "e"; "b"; "f#"; "c#"; "g#"; "d#" ] |> Set.ofList

let chromatic (tonic:string) =
    let getNotes tonic =
        if usesSharps |> Set.exists ((=) tonic) then
            sharps @ sharps
        else
            flats @ flats
    let notes = getNotes tonic
    match List.tryFindIndex ((=) tonic) notes with
    | None -> [ ]
    | Some index -> notes.[index..] |> List.take 12

let interval intervals tonic = failwith "You need to implement this function."