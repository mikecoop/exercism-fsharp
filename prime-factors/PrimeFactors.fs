module PrimeFactors

let factors (number:int64) : int list =
    let rec primeFactors factor = function
    | number when number % factor = 0L ->
        factor :: (primeFactors factor (number / factor))
    | number when number > factor ->
        primeFactors (factor + 1L) number
    | number when number > 1L ->
        [ number ]
    | _ -> [ ]
    primeFactors 2L number |> List.map int32