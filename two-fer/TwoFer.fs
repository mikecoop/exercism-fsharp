module TwoFer

let twoFer (input: string option): string =
    let name = input |> Option.defaultValue "you"
    sprintf "One for %s, one for me." name