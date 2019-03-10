module Series

let slices (str:string) length =
    let getResult length =
        str.ToCharArray()
        |> List.ofArray
        |> List.windowed length
        |> List.map List.toArray
        |> List.map System.String
    if length < 1 then
        None
    else
        let result = getResult length
        if result.Length = 0 then
            None
        else
            Some result