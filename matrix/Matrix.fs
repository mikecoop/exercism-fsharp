module Matrix

let parseMatrix (matrix:string) =
    matrix.Split('\n')
    |> List.ofArray
    |> List.map (fun s-> 
        s.Split(' ')
        |> List.ofArray
        |> List.map System.Int32.Parse)

let row index matrix =
    let parsed = parseMatrix matrix
    parsed.[index]

let column index matrix =
    let parsed = parseMatrix matrix
    parsed
    |> List.map (fun list -> list.[index])
