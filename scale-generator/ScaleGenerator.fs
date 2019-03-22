module ScaleGenerator

open System

let sharps = [ "A"; "A#"; "B"; "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#" ]

let flats = [ "A"; "Bb"; "B"; "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab" ]

let usesSharps = [ "C"; "G"; "D"; "A"; "E"; "B"; "F#"; "e"; "a"; "b"; "f#"; "c#"; "g#"; "d#" ] |> Set.ofList

let twoOctavesFromNote (note:string) =
    let stringEquals x y = System.String.Equals(x, y, StringComparison.InvariantCultureIgnoreCase)
    let octaves =
        if usesSharps |> Set.exists ((=) note) then
            sharps @ sharps @ sharps
        else
            flats @ flats @ sharps
    List.tryFindIndex (stringEquals note) octaves
    |> Option.map (fun i -> octaves.[i..] |> List.take 24)

let intervalsToIndices intervals =
    let intervalAmount = function
        | 'A' -> 3
        | 'M' -> 2
        | 'm' -> 1
        | _ -> 0

    let fold list interval = intervalAmount interval + (List.head list) :: list

    intervals
    |> List.ofSeq
    |> List.fold fold [ 0 ]
    |> List.skip 1
    |> List.rev

let chromatic (tonic:string) =
    match twoOctavesFromNote tonic with
    | None -> [ ]
    | Some notes -> notes |> List.take 12

let interval intervals tonic =
    match twoOctavesFromNote tonic with
    | None -> [ ]
    | Some notes ->
        let indices = intervalsToIndices intervals
        [ for i in indices -> notes.[i] ]