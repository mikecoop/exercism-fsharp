module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int = values |> List.rev |> List.head

let highest (values: int list): int = values |> List.max

let top (values: int list): int list = values |> List.sortDescending |> List.take (min 3 (List.length values))

let report (values: int list): string =
    let latest = latest values
    let highest = highest values
    (sprintf "Your latest score was %i. " latest) +
    if latest = highest then
        "That's your personal best!"
    else
        sprintf "That's %i short of your personal best!" (highest - latest)