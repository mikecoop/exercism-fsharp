module SaddlePoints


let saddlePoints matrix =
    let findIndicies (value:int) (list:int list) =
        list
        |> List.indexed
        |> List.filter (fun (_, v) -> v = value)
        |> List.map fst
    
    let minIndices (list:int list) =
        if list = List.empty then
            List.empty
        else
            findIndicies (List.min list) list
    
    let maxIndices (list:int list) =
        if list = List.empty then
            List.empty
        else
            findIndicies (List.max list) list
    
    let maxInRows matrix =
        matrix
        |> List.indexed
        |> List.collect (fun (rowIndex, row) ->
            maxIndices row
            |> List.map (fun columnIndex -> (rowIndex, columnIndex)))
    
    let minInColumns matrix =
        matrix
        |> List.transpose
        |> List.indexed
        |> List.collect (fun (columnIndex, column) ->
            minIndices column
            |> List.map (fun rowIndex -> (rowIndex, columnIndex)))

    let maxInRowCoords = maxInRows matrix |> Set.ofList
    let minInColCoords = minInColumns matrix |> Set.ofList
    Set.intersect maxInRowCoords minInColCoords |> Set.toList
