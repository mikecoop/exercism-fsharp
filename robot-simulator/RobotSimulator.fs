module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let modulus x m = (x % m + m) % m

let ( %% ) = modulus

let create direction position =
    { direction = direction; position = position }

let move (instructions:string) robot =
    let turn robot instruction =
        let directions = [ North; East; South; West ]
        let index = directions |> List.findIndex ((=) robot.direction)
        let newIndex =
            match instruction with
            | 'L' -> index - 1
            | 'R' -> index + 1
            | _ -> index
        (instruction, { robot with direction = directions.[newIndex %% 4] })
    
    let advance (instruction, robot) =
        if instruction = 'A' then
            let newPosition =
                let x, y = robot.position
                match robot.direction with
                | North -> (x, y + 1)
                | East -> (x + 1, y)
                | South -> (x, y - 1)
                | West -> (x - 1, y)
            { robot with position = newPosition }
        else
            robot
    
    let turnAndAdvance robot = (turn robot) >> advance

    instructions |> Seq.fold turnAndAdvance robot