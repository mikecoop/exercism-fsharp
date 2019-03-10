module Anagram

let findAnagrams sources (target:string) =
    let sortedCharArray (word:string) =
        Array.sort (word.ToUpper().ToCharArray())
    let isAnagram (word:string) =
        sortedCharArray word = sortedCharArray target &&
            word.ToUpper() <> target.ToUpper()
    sources
    |> List.filter isAnagram