module Triangle

let validTriangle triangle =
    match triangle with
    | [ x; y; z ] -> x + y > z && x + z > y && y + z > x
    | _ -> false

let distinctSides n triangle =
    let set = triangle |> Set.ofList
    set.Count = n &&
    set |> Set.forall (fun s -> s > 0.0)

let equilateral triangle =
    triangle |> validTriangle &&
    triangle |> distinctSides 1

let isosceles triangle =
    triangle |> validTriangle &&
    (triangle |> equilateral ||
     triangle |> distinctSides 2)
    
let scalene triangle =
    triangle |> validTriangle &&
    triangle |> distinctSides 3