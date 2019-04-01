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

let dieScore = function
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

let scoreDice dice =
    dice |> List.sumBy dieScore

let (|AllDiceSame|_|) (dice:Die list) =
    match dice with
    | head :: tail when tail |> List.forall ((=) head) -> Some AllDiceSame
    | _ -> None

let dieCounts dice = dice |> List.countBy id |> List.sortBy snd |> List.map snd

let (|DiceAreFullHouse|_|) (dice:Die list) =
    match dieCounts dice with
    | [ 2; 3 ] -> Some DiceAreFullHouse
    | _ -> None

let (|DiceAreFourOfAKind|_|) (dice:Die list) =
    match dieCounts dice with
    | [ 1; 4 ] -> Some DiceAreFourOfAKind
    | _ -> None

let dieCount die dice =
    dice |> List.filter ((=) die) |> List.length

let score category dice =
    match category, dice with
    | Yacht, AllDiceSame -> 50
    | FullHouse, DiceAreFullHouse -> scoreDice dice
    | Ones, _ -> dice |> dieCount One
    | Twos, _ -> (dice |> dieCount Two) * 2
    | Threes, _ -> (dice |> dieCount Three) * 3
    | Fours, _ -> (dice |> dieCount Four) * 4
    | Fives, _ -> (dice |> dieCount Five) * 5
    | Sixes, _ -> (dice |> dieCount Six) * 6
    | _ -> 0