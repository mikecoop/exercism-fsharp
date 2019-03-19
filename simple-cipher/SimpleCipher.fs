module SimpleCipher
open System

type SimpleCipher(key: string) =

    let charToIndex (c:char) =
        (int c) - (int 'a')

    let charFromIndex (i:int) =
        char (i + int 'a')

    let code (key:string) (text:string) f =
        let length = Seq.length text
        let mod26 x = (x % 26 + 26) % 26
        Seq.zip key.[..length - 1] text
        |> Seq.map ((fun (keyChar, textChar) -> f (charToIndex textChar) (charToIndex keyChar)) >> mod26 >> charFromIndex)
        |> Seq.toArray
        |> String

    member __.Key with get() = if String.length key < 100 then String.replicate (100 / String.length key) key else key

    member this.Encode(plaintext) =
        code this.Key plaintext (+)

    member this.Decode(ciphertext) =
        code this.Key ciphertext (-)

    new() =
        let randomKey =
            let rnd = Random()
            let chars = [ 'a' .. 'z' ]
            [ for _ in 1 .. 100 ->
                chars.[rnd.Next(26)] ]
            |> List.toArray
        SimpleCipher(String randomKey)