module BookStore

let counts list =
    list |> List.countBy id

let subtractOneOfEach list =
    match list with
    | [ ] -> [ ]
    | _ ->
        list
        |> List.map (fun (value, count) -> value, count - 1)
        |> List.filter (fun (_, count) -> count <> 0)

let discount = function
    | 5 -> 0.75
    | 4 -> 0.80
    | 3 -> 0.90
    | 2 -> 0.95
    | _ -> 1.00

let rec totalPrice acc list =
    match list with
    | [ ] -> acc
    | _ ->
        let uniqueCount = list |> List.length
        let newAcc = acc + (float uniqueCount * 8.00 * discount uniqueCount)
        let newList = list |> subtractOneOfEach
        totalPrice newAcc newList

let total books =
    let pairs = counts books
    totalPrice 0.0 pairs

[1; 1; 2; 2; 3; 3; 4; 5] |> counts