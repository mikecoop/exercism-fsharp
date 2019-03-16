module RomanNumerals

type PlaceValue = {
    Low: string
    Middle: string
    High: string
    GetDigit: int -> int
}

let placeValue number value =
    let digit = value.GetDigit number
    match digit with
    | 1 | 2 | 3 -> String.replicate digit value.Low
    | 4 -> value.Low + value.Middle
    | 5 -> value.Middle
    | 6 | 7 | 8 -> value.Middle + String.replicate (digit - 5) value.Low
    | 9 -> value.Low + value.High
    | _ -> ""

let thousands = { Low = "M"; Middle = ""; High = ""; GetDigit = fun n -> n / 1000 }
let hundreds = { Low = "C"; Middle = "D"; High = "M"; GetDigit = fun n -> (n % 1000) / 100 }
let tens = { Low = "X"; Middle = "L"; High = "C"; GetDigit = fun n -> (n % 100) / 10 }
let ones = { Low = "I"; Middle = "V"; High = "X"; GetDigit = fun n -> n % 10 }

let roman arabicNumeral =
    let getPlaceValue = placeValue arabicNumeral
    getPlaceValue thousands +
    getPlaceValue hundreds +
    getPlaceValue tens +
    getPlaceValue ones