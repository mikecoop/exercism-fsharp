module House

let recite startVerse endVerse: string list =
    let verses =
        [ "the horse and the hound and the horn that belonged to"
          "the farmer sowing his corn that kept"
          "the rooster that crowed in the morn that woke"
          "the priest all shaven and shorn that married"
          "the man all tattered and torn that kissed"
          "the maiden all forlorn that milked"
          "the cow with the crumpled horn that tossed"
          "the dog that worried"
          "the cat that killed"
          "the rat that ate"
          "the malt that lay"
          "in the house that Jack built" ]

    let part number =
        if number = 1 then
            System.String (Seq.skip 3 verses.[verses.Length - 1] |> Seq.toArray)
        else
            verses.[verses.Length - number .. verses.Length - 1]
            |> String.concat " "

    let getVerse v = sprintf "This is %s." (part v)
    
    [ for v in startVerse .. endVerse do
        yield getVerse v ]