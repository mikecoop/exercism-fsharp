module Yacht

type Category =
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

let dieValue = function
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

type DieCount = { Die:Die; Count:int }

let dieCounts dice =
    dice
    |> List.countBy id
    |> List.sortByDescending snd
    |> List.map (fun (die, count) -> { Die = die; Count = count })

let yachtScore dice =
    if dice |> dieCounts |> List.length = 1 then 50 else 0

let fourOfAKindScore dice =
    let firstCount = dice |> dieCounts |> List.head
    if firstCount.Count >= 4 then dieValue firstCount.Die * 4 else 0

let fullHouseScore dice =
    let firstCount = dice |> dieCounts |> List.head
    if firstCount.Count = 3 then dice |> List.sumBy dieValue else 0

let diceAreConsecutiveWithout missing dice =
    let counts = dice |> dieCounts |> List.map (fun count -> count.Die)
    List.length counts = 5 && not (dice |> List.contains missing)

let littleStraightScore dice =
    if dice |> diceAreConsecutiveWithout Six then 30 else 0

let bigStraightScore dice =
    if dice |> diceAreConsecutiveWithout One then 30 else 0

let choiceScore dice =
    dice |> List.sumBy dieValue

let singleValueScore die dice =
    let count = dice |> List.filter ((=) die) |> List.length
    dieValue die * count

let score category dice =
    match category with
    | Yacht -> yachtScore dice
    | FourOfAKind -> fourOfAKindScore dice
    | FullHouse -> fullHouseScore dice
    | LittleStraight -> littleStraightScore dice
    | BigStraight -> bigStraightScore dice
    | Choice -> choiceScore dice
    | Ones -> dice |> singleValueScore One
    | Twos -> dice |> singleValueScore Two
    | Threes -> dice |> singleValueScore Three
    | Fours -> dice |> singleValueScore Four
    | Fives -> dice |> singleValueScore Five
    | Sixes -> dice |> singleValueScore Six