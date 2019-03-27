module Markdown

open System.Text.RegularExpressions

let wrapWithTag tag str =
    sprintf "<%s>%s</%s>" tag str tag

let replaceWithTags search tag str =
    let pattern = sprintf "%s(.+?)%s" search search
    let replacement = sprintf "<%s>$1</%s>" tag tag
    Regex.Replace(str, pattern, replacement)

let replacePoundsWithHeaders (line:string) =
    let poundCount = line |> Seq.takeWhile ((=) '#') |> Seq.length
    let restOfLine = line.[poundCount + 1 ..]
    wrapWithTag ("h" + poundCount.ToString()) restOfLine

let replaceInlineTags (line:string) =
    line
    |> replaceWithTags "__" "strong"
    |> replaceWithTags "_" "em"

let isListItem (line:string) =
    line.StartsWith("* ")

let isHeader (line:string) =
    line.StartsWith "#"

let parseLine (line:string) =
    if isListItem line then
        wrapWithTag "li" (replaceInlineTags line.[2..])
    elif isHeader line then
        replacePoundsWithHeaders line
    else
        wrapWithTag "p" (replaceInlineTags line)

let parseLines (lines:string[]) =
    let firstListItem =
        match (lines |> Array.tryFindIndex isListItem) with
        | Some index -> index
        | None -> -1

    let parsedLines =
        lines
        |> List.ofArray
        |> List.mapi
            (fun index line ->
                if index = firstListItem then "<ul>" else ""
                + parseLine line)

    let isList = firstListItem <> -1
    parsedLines @ if isList then [ "</ul>" ] else [ ]

let rec parse (markdown: string) =
    markdown.Split('\n') |> parseLines |> List.reduce (+)