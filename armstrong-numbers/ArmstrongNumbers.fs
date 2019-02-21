module ArmstrongNumbers

open System

let isArmstrongNumber (number: int): bool =
    let digits = (string number) |> List.ofSeq |> List.map (fun c -> (string c) |> Int32.Parse)
    let power n = pown n (List.length digits)
    let sumOfSquares = digits |> List.sumBy power
    number = sumOfSquares