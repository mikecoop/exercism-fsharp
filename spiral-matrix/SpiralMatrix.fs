module SpiralMatrix

let reverseAllAndTranspose = List.map List.rev >> List.transpose

let transposeAndReverseAll = List.transpose >> List.map List.rev

let rec chunks count list : int list list =
    let take n list = (list |> List.take n), (list |> List.skip n)
    match count with
    | 0 -> [ ]
    | count when count = List.length list / count ->
        let (chunk, rest) = take count list
        chunk :: chunks (count - 1) rest
    | _ ->
        let (chunk1, rest1) = take count list
        let (chunk2, rest2) = take count rest1
        [ chunk1; chunk2 ] @ chunks (count - 1 ) rest2

let rec buildMatrix matrix chunks =
    match chunks with
    | [ ] -> matrix |> reverseAllAndTranspose
    | chunk :: rest ->
        let newMatrix = chunk :: matrix |> transposeAndReverseAll
        buildMatrix newMatrix rest

let spiralMatrix (size:int) : int list list =
    let chunksReversed = chunks size [ 1 .. size * size ] |> List.rev
    buildMatrix [ ] chunksReversed