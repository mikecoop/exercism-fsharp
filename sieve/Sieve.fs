module Sieve

let primes limit =
    let notMarked (marked, _) = marked = false
    let range = [ 2 .. limit ]
    let unmarked = range |> List.map (fun value -> (false, value))
    
    let folder (pairs:(bool * int) list) prime =
        let IsMultiple value = value % prime = 0
        let markValue value = (IsMultiple value) && prime <> value
        pairs
        |> List.map (fun (marked, value) -> (marked || markValue value, value))
    
    range
    |> List.fold folder unmarked
    |> List.filter notMarked
    |> List.map snd