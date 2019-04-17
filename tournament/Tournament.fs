module Tournament

type MatchOutcome =
    | Win
    | Draw
    | Loss

type MatchResult = { TeamName: string; Outcome: MatchOutcome }

type TeamResults =
    { TeamName: string
      Played: int
      Wins: int
      Draws: int
      Losses: int
      Points: int }

let teamResults name =
    { TeamName = name
      Played = 0
      Wins = 0
      Draws = 0
      Losses = 0
      Points = 0 }

let addWin teamResults =
    { teamResults with
        Played = teamResults.Played + 1
        Wins = teamResults.Wins + 1
        Points = teamResults.Points + 3 }

let addDraw teamResults =
    { teamResults with
        Played = teamResults.Played + 1
        Draws = teamResults.Draws + 1
        Points = teamResults.Points + 1 }

let addLoss teamResults =
    { teamResults with
        Played = teamResults.Played + 1
        Losses = teamResults.Losses + 1 }

let addResult result teamResults =
    match result with
    | Win -> addWin teamResults
    | Draw -> addDraw teamResults
    | Loss -> addLoss teamResults

let printFormatted name played wins draws losses points =
    let spaces str amount = String.replicate (amount - String.length str) " "
    let addSpacesAfter str amount =
        str + spaces str amount
    let addSpacesBefore str amount =
        spaces str amount + str

    addSpacesAfter name 31
    + "| " + addSpacesBefore played 2
    + " | " + addSpacesBefore wins 2
    + " | " + addSpacesBefore draws 2
    + " | " + addSpacesBefore losses 2
    + " | " + addSpacesBefore points 2

let printFormattedResult teamResults =
    printFormatted
        teamResults.TeamName
        (string teamResults.Played)
        (string teamResults.Wins)
        (string teamResults.Draws)
        (string teamResults.Losses)
        (string teamResults.Points)

let parseInput (input:string) =
    let vals = input.Split(';') |> List.ofArray
    match vals with
    | [ team1; team2; outcome ] ->
        match outcome with
        | "win" ->
            { TeamName = team1; Outcome = Win },
            { TeamName = team2; Outcome = Loss}
        | "draw" ->
            { TeamName = team1; Outcome = Draw },
            { TeamName = team2; Outcome = Draw }
        | "loss" ->
            { TeamName = team1; Outcome = Loss },
            { TeamName = team2; Outcome = Win}
        | _ -> failwith "Invalid match outcome"
    | _ -> failwith "Invalid input format"

let folder (map:Map<string, TeamResults>) (input:string) : Map<string, TeamResults> =
    let addOrUpdateMap (teamName:string) matchResult map =
        let results =
            if map |> Map.containsKey (teamName.ToUpper()) then
                map |> Map.find (teamName.ToUpper())
            else
                teamResults teamName
        map |> Map.add (teamName.ToUpper()) (results |> addResult matchResult)

    let team1, team2 = parseInput input
    map
    |> addOrUpdateMap team1.TeamName team1.Outcome
    |> addOrUpdateMap team2.TeamName team2.Outcome

let tally input =
    let header = printFormatted "Team" "MP" "W" "D" "L" "P"
    let teamResults =
        input
        |> List.fold folder Map.empty
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun result -> -result.Wins, result.Losses)
    header :: (teamResults |> List.map printFormattedResult)