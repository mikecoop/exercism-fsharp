module Change

let rec change acc coins target =
    match coins with
    | [ ] -> [ ]
    | coin :: rest ->
        let remainder = target - coin
        let newAcc = coin :: acc
        if remainder = 0 then
            newAcc
        elif remainder < 0 then
            change acc rest target
        elif coin <= target then
            change newAcc coins remainder
        else
            change newAcc rest remainder

let findFewestCoins coins target =
    if target = 0 then
        Some List.empty
    else
        let bigToSmall = coins |> List.rev
        [ for i in 0 .. (List.length bigToSmall) - 1 do
            yield change [ ] (List.skip i bigToSmall) target ]
        |> List.where (List.isEmpty >> not)
        |> List.sortBy List.length
        |> List.tryHead