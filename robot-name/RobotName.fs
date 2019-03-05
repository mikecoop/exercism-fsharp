module RobotName

type Robot = { Name: string }

let getName() =
    let random = System.Random()
    let randomLetter =
        [ 'A' .. 'Z' ].[random.Next(26)]
    let randomNumber =
        random.Next(10)
    sprintf "%c%c%i%i%i" randomLetter randomLetter randomNumber randomNumber randomNumber

let mkRobot() = { Name = getName() }

let name robot = robot.Name

let reset robot = { robot with Name = getName() }