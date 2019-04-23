module PalindromeProducts

let isPalindrome n =
    let rec rev acc = function
        | 0 -> acc
        | x -> rev (acc * 10 + (x % 10)) (x / 10)
    n = rev 0 n

let palindromeProductFactors minFactor maxFactor =
    [ for x in [ minFactor .. maxFactor ] do
        for y in [ x .. maxFactor ] do
            let product = x * y
            if isPalindrome product
            then yield (product, (x, y)) ]

let selectFactor minFactor maxFactor fSelector =
    match palindromeProductFactors minFactor maxFactor with
    | [ ] -> None
    | factors ->
        factors
        |> List.groupBy fst
        |> List.map (fun (factor, values) -> factor, values |> List.map snd)
        |> fSelector fst
        |> Some

let largest minFactor maxFactor =
    selectFactor minFactor maxFactor List.maxBy

let smallest minFactor maxFactor =
    selectFactor minFactor maxFactor List.minBy