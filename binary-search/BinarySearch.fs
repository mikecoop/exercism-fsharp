module BinarySearch

let find (input:int[]) (value:int) =
    let rec findRec pairs (value:int) =
        match Array.length pairs with
            | 0 ->
                None
            | 1 when snd pairs.[0] <> value ->
                None
            | i ->  let middle = i / 2
                    match snd pairs.[middle] with
                    | v when v > value ->
                        findRec pairs.[..middle - 1] value
                    | v when v < value ->
                        findRec pairs.[middle + 1..] value
                    | _ -> Some (fst pairs.[middle])
    let pairs = Array.indexed input
    findRec pairs value