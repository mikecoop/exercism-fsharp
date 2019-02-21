module CollatzConjecture

let steps (number: int): int option =
    let rec innerFn stepNumber number =
        if number < 1 then
            None
        elif number = 1 then
            Some stepNumber
        elif number % 2 = 0 then
            innerFn (stepNumber + 1) (number / 2)
        else
            innerFn (stepNumber + 1) (3 * number + 1)
    innerFn 0 number