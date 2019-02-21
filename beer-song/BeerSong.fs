module BeerSong

let recite (startBottles: int) (takeDown: int) =
    let verses bottles = 
        match bottles with
        | 0 -> 
            ( "No more bottles of beer on the wall, no more bottles of beer.",
              "Go to the store and buy some more, 99 bottles of beer on the wall." )
        | 1 ->
            ( "1 bottle of beer on the wall, 1 bottle of beer.",
              "Take it down and pass it around, no more bottles of beer on the wall." )
        | 2 ->
            ( "2 bottles of beer on the wall, 2 bottles of beer.",
              "Take one down and pass it around, 1 bottle of beer on the wall." )
        | bottles -> 
            ( sprintf "%i bottles of beer on the wall, %i bottles of beer." bottles bottles,
              sprintf "Take one down and pass it around, %i bottles of beer on the wall." (bottles - 1) )
    let endBottles = (startBottles - takeDown + 1)
    [ for bottles in startBottles .. -1 .. endBottles do
        let firstVerse, secondVerse = verses bottles
        yield firstVerse
        yield secondVerse
        if bottles <> endBottles then
            yield "" ]