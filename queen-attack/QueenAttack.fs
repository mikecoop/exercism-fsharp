module QueenAttack

open System

let create (position: int * int) =
    let inRange i = 0 <= i && i < 8
    inRange (fst position) && inRange (snd position)

let canAttack (queen1: int * int) (queen2: int * int) =
    let x1, y1 = queen1
    let x2, y2 = queen2
    (x1 = x2) || (y1 = y2) || Math.Abs((y1 - y2) / (x1 - x2)) = 1