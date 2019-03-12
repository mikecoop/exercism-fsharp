module PythagoreanTriplet

open System

let tripletsWithSum sum =
    [ for b in 1 .. sum - 1 do
        for a in 1 .. b - 1 do
            let c = Math.Sqrt(float (a * a + b * b))
            if c % 1.0 = 0.0 && a + b + int c = sum then
                yield (a, b, int c) ]
    |> List.sortBy (fun (a, _, _) -> a)