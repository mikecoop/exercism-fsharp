module Hamming

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length <> strand2.Length then None
    else
        Some (Seq.fold2
                (fun sum char1 char2 ->
                    if char1 <> char2 then sum + 1
                    else sum)
                0
                strand1
                strand2)
