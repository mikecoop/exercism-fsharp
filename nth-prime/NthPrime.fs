module NthPrime

let divisibleBy x y = x % y = 0

let isPrime previousPrimes p =
    if previousPrimes |> List.exists (fun prime -> divisibleBy p prime) then
        None
    else
        Some p

let rec primes count previousPrimes =
    if List.length previousPrimes = count then
        previousPrimes
    else
        let lastPrime = List.head previousPrimes
        let nextPrime =
            seq { lastPrime + 1 .. lastPrime + 1000 }
            |> Seq.pick (isPrime previousPrimes)
        primes count (nextPrime :: previousPrimes)

let prime nth : int option =
    match nth with
    | 0 -> None
    | count -> Some (primes count [ 2 ] |> List.head)