module FoodChain

let items =
    [ "fly", ""
      "spider", "It wriggled and jiggled and tickled inside her."
      "bird", "How absurd to swallow a bird!"
      "cat", "Imagine that, to swallow a cat!"
      "dog", "What a hog, to swallow a dog!"
      "goat", "Just opened her throat and swallowed a goat!"
      "cow", "I don't know how she swallowed a cow!"
      "horse", "" ]

let reasons =
    [ "I don't know how she swallowed a cow!"
      "She swallowed the cow to catch the goat."
      "She swallowed the goat to catch the dog."
      "She swallowed the dog to catch the cat."
      "She swallowed the cat to catch the bird."
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      "She swallowed the spider to catch the fly."
      "I don't know why she swallowed the fly. Perhaps she'll die." ]

let verseStart number =
    let (item, second) = List.item (number - 1) items
    [ "I know an old lady who swallowed a " + item + "." ] @
    match second with
    | "" -> [ ]
    | str -> [ str ]

let verseEnd number =
    match number with
    | 8 -> [ "She's dead, of course!" ]
    | _ ->
        reasons
        |> List.skip (List.length reasons - number)
        |> List.take number

let verse number = verseStart number @ verseEnd number

let recite start stop =
    [ for n in start .. stop ->
        verse n ]
    |> List.reduce (fun x y -> x @ "" :: y)