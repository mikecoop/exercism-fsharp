module Bob

let response (input: string): string =
    let isWhitespace (s: string) = (String.filter System.Char.IsWhiteSpace s) = s
    let hasLetters (s: string) = String.exists System.Char.IsLetter s
    let isAllCaps (s: string) = s = s.ToUpper() && hasLetters s
    let isQuestion (s: string) = s.Trim().EndsWith "?"

    match input with
    | s when isWhitespace s -> "Fine. Be that way!"
    | s when isAllCaps s && isQuestion s -> "Calm down, I know what I'm doing!"
    | s when isAllCaps s -> "Whoa, chill out!"
    | s when isQuestion s -> "Sure."
    | _ -> "Whatever."