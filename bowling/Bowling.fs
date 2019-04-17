module Bowling

type Roll = Roll of int

type Frame =
| Incomplete of Roll
| Open of Roll * Roll
| Spare of Roll * Roll
| Strike
| Invalid

type GameState =
| Active of Game
| SpareBonus of Game
| StrikeBonus of Game
| Completed of Game
| Invalid
and Game = { Frames: Frame List; BonusRolls: Roll list }

let getRolls = function
    | Open (Roll r1, Roll r2) -> [ r1; r2 ]
    | Spare (Roll r1, Roll r2) -> [ r1; r2 ]
    | Strike -> [ 10 ]
    | Frame.Invalid -> [ 0 ]
    | Incomplete _ -> failwith "Cannot get rolls from incomplete frame."

let nextFrame (roll:Roll) (previous:Frame option) : Frame =
    if roll < Roll 0 || roll > Roll 10 then
        Frame.Invalid
    else
        match previous, roll with
        | Some (Incomplete _ ), Roll 10 ->
            failwith "Total pins for frame cannot exceed 10."
        | _, Roll 10 ->
            Strike
        | Some (Incomplete (Roll r1)), Roll r2 when r1 + r2 = 10 ->
            Spare (Roll r1, Roll r2)
        | Some (Incomplete (Roll r1)), Roll r2 when r1 + r2 < 10 ->
            Open (Roll r1, Roll r2)
        | _, _ ->
            Incomplete roll

let previousFrames (frames:Frame list) : Frame list =
    match List.tryHead frames with
    | Some (Incomplete _) ->
        List.tail frames
    | _ ->
        frames

let gameFromFrames (frames:Frame list) : Game =
    { Frames = frames; BonusRolls = List.empty }

let newGame() : GameState = Active (gameFromFrames (List.empty))

let activeGameRoll (roll:Roll) (game:Game) : GameState =
    let previousFrame = List.tryHead game.Frames
    let frame = nextFrame roll previousFrame
    match frame with
    | Frame.Invalid ->
        GameState.Invalid
    | _ ->
        let frames = frame :: previousFrames game.Frames
        if List.length frames = 10 then
            match frame with
            | Incomplete _ ->
                Active (gameFromFrames (frames))
            | Spare _ ->
                SpareBonus (gameFromFrames (frames))
            | Strike _ ->
                StrikeBonus (gameFromFrames (frames))
            | Open _ ->
                Completed (gameFromFrames (frames))
            | Frame.Invalid ->
                GameState.Invalid
        else
            Active (gameFromFrames (frames))

let roll (pins:int) (game:GameState) : GameState =
    let roll = Roll pins
    match game with
    | Active active ->
        activeGameRoll roll active
    | SpareBonus bonus ->
        Completed { Frames = bonus.Frames; BonusRolls = [ roll ] }
    | StrikeBonus bonus ->
        match bonus.BonusRolls with
        | [ ] ->
            StrikeBonus { bonus with BonusRolls = [ roll ] }
        | [ Roll roll1 ] when
            (roll1 = 10 || roll1 + pins <= 10) &&
            (0 < pins && pins <= 10) ->
            Completed { bonus with BonusRolls = roll :: bonus.BonusRolls }
        | _ ->
            GameState.Invalid
    | _ ->
        GameState.Invalid

let framesWithRolls (game:Game) =
    let eightToOne =
        game.Frames
        |> List.windowed 3
        |> List.map List.rev
        |> List.choose (fun list ->
            match list with
            | first :: rest -> Some (first, List.collect getRolls rest)
            | _ -> None)
    let lastFrame = List.head game.Frames
    let bonusRolls = game.BonusRolls |> List.map (fun (Roll r) -> r) |> List.rev
    let last = (lastFrame, bonusRolls)
    let secondLastFrame = List.head (game.Frames |> List.skip 1)
    let secondLast = (secondLastFrame, getRolls lastFrame @ bonusRolls)
    last :: secondLast :: eightToOne

let frameScores framesWithRolls =
    framesWithRolls
    |> List.map (fun (frame, rolls) ->
        match frame with
        | Open (Roll roll1, Roll roll2) -> roll1 + roll2
        | Spare _ -> 10 + (rolls |> List.take 1 |> List.sum)
        | Strike _ -> 10 + (rolls |> List.take 2 |> List.sum)
        | _ -> 0)

let score (game:GameState) : int option =
    match game with
    | Completed completed ->
        Some (completed |> framesWithRolls |> frameScores |> List.sum)
    | _ ->
        None