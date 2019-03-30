module BookStore

let discount = function
    | 5 -> 0.75
    | 4 -> 0.80
    | 3 -> 0.90
    | 2 -> 0.95
    | _ -> 1.00

let getBookCounts books =
    books |> List.countBy id |> List.sortByDescending snd

let removeOneOfEachBook bookCounts =
    bookCounts
    |> List.map (fun (bookNumber, count) -> bookNumber, count - 1)
    |> List.filter (fun (_, count) -> count <> 0)

let removeGroupOf groupCount bookCounts =
    if List.length bookCounts < groupCount then
        removeOneOfEachBook bookCounts
    else
        (bookCounts
         |> List.take groupCount
         |> removeOneOfEachBook )
         @ (bookCounts |> List.skip groupCount)

let rec totalPrice acc maxGroupSize bookCounts =
    match bookCounts with
    | [ ] -> acc
    | _ ->
        let uniqueBooks = min (bookCounts |> List.length) maxGroupSize
        let newAcc = acc + (float uniqueBooks * 8.00 * discount uniqueBooks)
        totalPrice newAcc uniqueBooks (bookCounts |> removeGroupOf uniqueBooks)

let total books =
    let bookCounts = getBookCounts books
    let prices =
        [ for groupSize in 1 .. List.length bookCounts ->
            totalPrice 0.0 groupSize bookCounts ]
    if prices |> List.isEmpty then
        0.0
    else
        List.min prices