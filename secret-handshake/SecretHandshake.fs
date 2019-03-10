module SecretHandshake

let commands number =
    let bitSet b n = n &&& b = b
    let list =
        [ 1, "wink"
          2, "double blink"
          4, "close your eyes"
          8, "jump" ]
        |> List.choose (fun (b, s) ->
            if number |> bitSet b then
                Some s
            else
                None)
    if number |> bitSet 16 then
        list |> List.rev
    else
        list
    