module Acronym

let abbreviate (phrase:string) =
    phrase.ToLower()
    |> System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase
    |> String.filter System.Char.IsUpper