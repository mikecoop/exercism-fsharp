module PerfectNumbers

type Classification = Perfect | Abundant | Deficient 

let classify n : Classification option =
    if n <= 0 then
        None
    else
        let factors = [ 1 .. n - 1 ] |> List.filter (fun f -> n % f = 0)
        let sum = List.sum factors
        if sum = n then
            Some Perfect
        elif sum > n then
            Some Abundant
        else
            Some Deficient
