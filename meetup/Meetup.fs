module Meetup

open System

type Week = First | Second | Third | Fourth | Last | Teenth

let meetup year month week dayOfWeek =
    let getDates week =
        let daysInMonth = DateTime.DaysInMonth(year, month)
        let weekDates = [ 1 .. daysInMonth ] |> List.chunkBySize 7
        match week with
        | First -> weekDates.[0]
        | Second -> weekDates.[1]
        | Third -> weekDates.[2]
        | Fourth -> weekDates.[3]
        | Last -> [ daysInMonth - 6 .. daysInMonth]
        | Teenth -> [ 13 .. 19]

    getDates week
    |> List.map (fun day -> DateTime(year, month, day))
    |> List.find (fun date -> date.DayOfWeek = dayOfWeek)