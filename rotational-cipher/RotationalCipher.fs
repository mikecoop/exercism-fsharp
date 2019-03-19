module RotationalCipher

let rotate shiftKey text =
    let mod26 x = x % 26
    let charToIndex c = (int c) - (int 'a')
    let indexToChar i = char (i + int 'a')
    let shiftChar c = indexToChar (mod26 (charToIndex c + shiftKey))
    text
    |> String.map (function
        | c when System.Char.IsUpper c ->
            System.Char.ToUpper(shiftChar (System.Char.ToLower(c)))
        | c when System.Char.IsLetter c ->
            shiftChar c
        | c -> c)