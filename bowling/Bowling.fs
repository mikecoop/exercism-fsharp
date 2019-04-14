module Bowling

type Roll = Roll of int

type Frame =
| Incomplete of Roll
| Open of Roll * Roll
| Spare of Roll * Roll
| Strike

type Game =
| Active of Active
| Completed of Completed
and Active = { Frames: Frame list }
and Completed = { Frames: Frame List; BonusRolls: Roll list }

let getRolls = function
    | Open (r1, r2) -> [ r1; r2 ]
    | Spare (r1, r2) -> [ r1; r2 ]
    | Strike -> [ Roll 10 ]
    | Incomplete _ -> failwith "Cannot get rolls from incomplete frame."

let nextFrame (roll:Roll) (previous:Frame option) : Frame =
    if roll < Roll 0 || roll > Roll 10 then
        failwith "Roll cannot exceed 10 pins."
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

let newGame() : Game = Active { Frames = List.empty }

let roll (pins:int) (game:Game) : Game =
    match game with
    | Active active ->
        let previousFrame = List.tryHead active.Frames
        let frame = nextFrame (Roll pins) previousFrame
        let frames = frame :: previousFrames active.Frames
        if List.length frames = 10 then
            match frame with
            | Incomplete _ ->
                Active { Frames = frames }
            | _ ->
                Completed { Frames = frames; BonusRolls = List.empty }
        else
            Active { Frames = frames }
    | Completed _ ->
        failwith "Game is already complete."

let score (game:Game) : int option =
    match game with
    | Completed completed ->
        let rolls =
            (completed.BonusRolls |> List.map (fun (Roll r) -> r)) @
            (List.collect getRolls completed.Frames
            |> List.map (fun (Roll r) -> r))
        Some (List.sum rolls)
    | _ ->
        failwith "Cannot score incomplete game."