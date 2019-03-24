module Ledger

open System
open System.Globalization

type Entry = { Date: DateTime; Description: string; Change: int }

type Locale =
    | EnglishUS
    | DutchNetherlands

type Currency =
    | USDollars
    | Euro

let getLocale = function
    | "en-US" -> EnglishUS
    | "nl-NL" -> DutchNetherlands
    | _ -> failwith "Unknown locale"

let getCurrency = function
    | "USD" -> USDollars
    | "EUR" -> Euro
    | _ -> failwith "Unknown currency"

let getCurrencySymbol = function
    | USDollars -> "$"
    | Euro -> "€"

let getCultureInfo = function
    | EnglishUS -> CultureInfo ("en-US")
    | DutchNetherlands -> CultureInfo ("nl-NL")

let getHeader = function
    | EnglishUS ->
        "Date       | Description               | Change       "
    | DutchNetherlands ->
        "Datum      | Omschrijving              | Verandering  "

let getDateFormat = function
    | EnglishUS -> "MM\/dd\/yyyy"
    | DutchNetherlands -> "dd-MM-yyyy"

let formatDescription (description:string) =
    if description.Length <= 25 then
        description.PadRight(25)
    elif description.Length = 25 then
        description
    else
        description.[0..21] + "..."

let formatCurrency locale currency (amount:float) =
    let isNegative = amount < 0.0
    let formatValue locale (amount:float) =
        if locale = DutchNetherlands then " " else ""
        + amount.ToString("#,#0.00", getCultureInfo locale)
    let currencySymbol = getCurrencySymbol currency
    let formattedValue = formatValue locale amount
    match locale, isNegative with
    | EnglishUS, true ->
        "(" + currencySymbol + formattedValue.Substring(1) + ")"
    | _, true ->
        currencySymbol + formattedValue
    | _, false ->
        currencySymbol + formattedValue + " "

let mkEntry date description change = { Date = DateTime.Parse(date, CultureInfo.InvariantCulture); Description = description; Change = change }

let getFormattedEntry locale currency entry =
    let date = entry.Date.ToString (getDateFormat locale)
    let description = formatDescription entry.Description
    let change = float entry.Change / 100.0
    let amount = (formatCurrency locale currency change).PadLeft(13)

    "\n" + date + " | " + description + " | " + amount

let getFormattedEntries locale currency entries =
    if entries = List.empty then
        ""
    else
        (entries
        |> List.sortBy (fun x -> x.Date, x.Description, x.Change)
        |> List.map (getFormattedEntry locale currency)
        |> List.reduce (+))

let formatLedger currencyCode localeCode entries =
    let currency = getCurrency currencyCode
    let locale = getLocale localeCode
    getHeader locale + getFormattedEntries locale currency entries