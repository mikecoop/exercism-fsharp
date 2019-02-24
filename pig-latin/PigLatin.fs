module PigLatin

let translate (input:string) =
    let getWord (input:string) =
        let sounds = [ "thr"; "sch"; "ch"; "qu"; "th"; "sh"; "rh" ]
        let exceptions = [ "a"; "e"; "i"; "o"; "u"; "yt"; "xr"; ]
        
        if exceptions |> List.exists input.StartsWith then
            input
        elif sounds |> List.exists input.StartsWith then
            let sound = sounds |> List.filter (input.StartsWith) |> List.head
            input.[sound.Length..] + sound
        elif input.[1..].StartsWith("qu") then
            input.[3..] + input.[0..2]
        else
            input.[1..] + string input.[0]

    input.Split(' ')
    |> List.ofArray
    |> List.map getWord
    |> List.map (fun w -> w + "ay")
    |> String.concat " "