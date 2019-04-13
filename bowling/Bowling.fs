module Bowling

type CompletedFrame =
| Open of int
| Spare
| Strike

type Frame = int option * int option

type Game =
    { CompletedFrames: CompletedFrame list
      CurrentFrame: Frame }

let newFrame() : Frame = (None, None)

let completedFrame (frame:Frame) =
    match frame with
    | Some 10, None
        -> Strike
    | Some r1, Some r2 when r1 + r2 = 10
        -> Spare
    | Some r1, Some r2 when r1 + r2 < 10
        -> Open (r1 + r2)
    | _ -> failwith "Invalid frame."

let nextFrame game =
    { game with
        CompletedFrames = completedFrame game.CurrentFrame :: game.CompletedFrames;
        CurrentFrame = newFrame() }

let moveToNextFrameIfComplete game =
    match game.CurrentFrame with
    | (Some 10, None)
    | (Some _, Some _) -> nextFrame game
    | _ -> game

let updateCurrentFrame (pins:int) (frame:Frame) : Frame =
    match frame with
    | None, None -> (Some pins, None)
    | Some roll1, None -> (Some roll1, Some pins)
    | _ -> failwith "Current frame is invalid."

let nextRoll (pins:int) (game:Game) : Game =
    { game with CurrentFrame = game.CurrentFrame |> updateCurrentFrame pins }

let newGame() : Game =
    { CompletedFrames = List.empty
      CurrentFrame = newFrame() }

let roll (pins:int) (game:Game) : Game =
    game |> nextRoll pins |> moveToNextFrameIfComplete

let score (game:Game) : int option =
    if List.length game.CompletedFrames > 10 then
        None
    else
        game.CompletedFrames
        |> List.sumBy (function
            | Open pins -> pins
            | _ -> 0)
        |> Some

let rollMany rolls game = List.fold (fun game pins -> roll pins game) game rolls
let rolls = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
let game = rollMany rolls (newGame())