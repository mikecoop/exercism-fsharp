module Leap

let leapYear (year: int): bool =
    let divisbleBy x y = x % y = 0
    divisbleBy year 4 && ((divisbleBy year 100) = false || divisbleBy year 400)