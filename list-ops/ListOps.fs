module ListOps

let rec foldl folder state = function
    | [ ] -> state
    | first :: rest ->
        let newState = folder state first
        foldl folder newState rest

let rec foldr folder state = function
    | [ ] -> state
    | first :: rest ->
        let newState = foldr folder state rest
        folder first newState

let length list =
    let folder count _ = count + 1
    foldl folder 0 list

let reverse list =
    let folder list item = item :: list
    foldl folder [ ] list

let map f list =
    let folder item list = (f item) :: list
    foldr folder [ ] list

let filter f list =
    let folder item list =
        if f item then
            item :: list
        else
            list
    foldr folder [ ] list

let append xs ys =
    let folder item list = item :: list
    foldr folder ys xs

let concat xs =
    foldr append [ ] xs