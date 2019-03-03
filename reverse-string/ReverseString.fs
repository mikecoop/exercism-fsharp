module ReverseString

open System

let reverse (input: string): string =
    let append (c:Char) (str:string) = str + (string c)
    Seq.foldBack append input ""