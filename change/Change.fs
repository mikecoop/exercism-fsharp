module Change

let rec change acc coins target =
    match coins with
    | [ ] -> acc
    | coin :: rest ->
        let remainder = target - coin
        if remainder = 0 then
            target :: acc
        elif remainder > 0 then
            change (target :: acc) rest target
        else
            [ ]

let findFewestCoins coins target =
    let bigToSmall = coins |> Seq.rev
    Some [ 25 ]

change [ ] (List.rev [1; 5; 10; 25; 100]) 25