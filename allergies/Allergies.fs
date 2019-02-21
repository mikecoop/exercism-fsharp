module Allergies

open System

type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let isInMask mask value = mask &&& value = value

let allergicTo codedAllergies (allergen:Allergen) =
    int allergen |> isInMask (codedAllergies)

let list codedAllergies =
    let allergyInMask = int >> isInMask codedAllergies
    [ Allergen.Eggs; Allergen.Peanuts; Allergen.Shellfish; Allergen.Strawberries
      Allergen.Tomatoes; Allergen.Chocolate; Allergen.Pollen; Allergen.Cats ]
    |> List.filter allergyInMask