module KindergartenGarden

type Plant =
    | Radishes
    | Clover
    | Grass
    | Violets

let plants (diagram:string) student =
    let index = 
        [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]
        |> List.findIndex ((=) student)
    let toPlant c =
        match c with
        | 'R' -> Radishes
        | 'C' -> Clover
        | 'G' -> Grass
        | 'V' -> Violets
        | _ -> failwith "Unknown plant type!"
    let getPlantsAtIndex index (plants:string) = plants.[ index * 2 .. (index * 2) + 1 ]

    let rows = diagram.Split('\n')
    (rows.[0] |> getPlantsAtIndex index) + (rows.[1] |> getPlantsAtIndex index)
    |> Seq.map toPlant |> List.ofSeq