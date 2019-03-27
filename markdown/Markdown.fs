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

let rec parse (markdown: string) =
   let mutable html = ""
   let mutable isList = false

   let lines = markdown.Split('\n')

   for line in lines do
       if line.StartsWith("* ") then
           if not isList then
               html <- html + "<ul>"
               isList <- true

           html <- html + wrapWithTag "li" (replaceInlineTags line.[2..])

       elif line.StartsWith "#" then
           html <- html + replacePoundsWithHeaders line
       else
           html <- html + wrapWithTag "p" (replaceInlineTags line)

   if isList then
       html <- html + "</ul>"

   html