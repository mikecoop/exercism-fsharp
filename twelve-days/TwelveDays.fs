module TwelveDays

let recite start stop =
    let allVerses =
        [ "first", "a Partridge in a Pear Tree"
          "second", "two Turtle Doves"
          "third", "three French Hens"
          "fourth", "four Calling Birds"
          "fifth", "five Gold Rings"
          "sixth", "six Geese-a-Laying"
          "seventh", "seven Swans-a-Swimming"
          "eighth", "eight Maids-a-Milking"
          "ninth", "nine Ladies Dancing"
          "tenth", "ten Lords-a-Leaping"
          "eleventh", "eleven Pipers Piping"
          "twelfth", "twelve Drummers Drumming" ]
    
    let getVerse n =
        let firstPart =
            let count = fst allVerses.[n - 1]
            sprintf "On the %s day of Christmas my true love gave to me: " count
        
        let secondPart =
            let items = allVerses.[..n - 1] |> List.map snd
            match items with
            | [ ] -> [ ]
            | [ s ] -> [ s ]
            | head :: tail ->
                ("and " + head) :: tail
            |> List.rev
            |> String.concat ", "
        firstPart + secondPart + "."
    
    [ for n in start .. stop do
        yield getVerse n ]
