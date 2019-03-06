module Seq

let keep pred (xs:'a seq) =
    let keepFolder pred item list = 
        if pred item then
            item :: list
        else
            list
    Seq.foldBack (keepFolder pred) xs List.empty
    |> List.toSeq

let discard pred xs =
    keep (pred >> not) xs