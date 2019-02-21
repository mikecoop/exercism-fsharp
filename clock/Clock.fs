module Clock

type Time = { Hours: int; Minutes: int }

let modulus x m = (x % m + m) % m

let ( %% ) = modulus

let updateClock hours minutes clock =
    let hoursFromMinutes =
        let minutes = minutes + clock.Minutes
        if minutes < 0 then
            (minutes / 60) - 1
        else
            minutes / 60
    let currentHours, currentMinutes = clock.Hours, clock.Minutes
    { Hours = (currentHours + hours + hoursFromMinutes) %% 24
      Minutes = (currentMinutes + minutes) %% 60 }

let create hours minutes =
    { Hours = 0; Minutes = 0 } |> updateClock hours minutes

let add minutes clock =
    clock |> updateClock 0 minutes

let subtract minutes clock =
    clock |> updateClock 0 -minutes

let display clock = sprintf "%02i:%02i" clock.Hours clock.Minutes