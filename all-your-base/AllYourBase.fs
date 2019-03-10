module AllYourBase

let rebase (digits:int list) inputBase outputBase =
    let fromDecimal outputBase num =
        let rec innerFn num outputBase =
            match num / outputBase, num % outputBase with
            | 0, r -> [ r ] 
            | d, r -> r :: innerFn d outputBase
        if outputBase < 2 then
            None
        else
            Some (innerFn num outputBase |> List.rev)
    let toDecimal inputBase digits =
        if digits |> List.exists (fun n -> n < 0 || n >= inputBase) || inputBase < 2 then
            None
        else
            digits
            |> List.rev
            |> List.indexed
            |> List.map (fun (i, n) -> n * pown inputBase i)
            |> List.sum
            |> Some
    let decimal = toDecimal inputBase digits
    decimal |> Option.bind (fromDecimal outputBase)
