module PhoneNumber

open System

let clean (input:string) : Result<UInt64, string> =
    let checkLength (s:string) =
        match s.Length with
        | s when s > 11 -> Error "more than 11 digits"
        | s when s < 10 -> Error "incorrect number of digits"
        | _ -> Ok s

    let checkInvalidChars (s:string) =
        let notValidPunctuation c = ([ '.'; '('; ')'; '-' ] |> List.contains c) = false
        if String.exists Char.IsLetter s then
            Error "alphanumerics not permitted"
        elif String.exists (fun c -> Char.IsPunctuation c && notValidPunctuation c) s then
            Error "punctuations not permitted"
        else
            Ok s

    let filterNumbers (s:string) =
        String.filter Char.IsNumber s

    let checkCountryCode (s:string) =
        match String.length s, s.[0] = '1' with
        | (11, false) -> Error "11 digits must start with 1"
        | (11, true) -> Ok s.[1..]
        | _ -> Ok s

    let checkDigitNotZeroOrOne n desc (s:string) =
        match s.[n] with
        | '0' -> Error (sprintf "%s cannot start with zero" desc)
        | '1' -> Error (sprintf "%s cannot start with one" desc)
        | _ -> Ok s

    let checkAreaCode = checkDigitNotZeroOrOne 0 "area code"
    let checkExchangeCode = checkDigitNotZeroOrOne 3 "exchange code"

    input
    |> checkInvalidChars
    |> Result.map filterNumbers
    |> Result.bind checkLength
    |> Result.bind checkCountryCode
    |> Result.bind checkAreaCode
    |> Result.bind checkExchangeCode
    |> Result.map UInt64.Parse