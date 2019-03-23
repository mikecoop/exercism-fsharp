module Grep

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Flags =
    { OutputFileNamesOnly: bool
      OutputLineNumbers: bool
      MatchCaseInsensitive: bool
      MatchEntireLine: bool
      MatchInverse: bool }

let parseFlags flagArguments =
    let flagsContain flag = flagArguments |> List.contains flag
    { OutputFileNamesOnly = flagsContain "-l"
      OutputLineNumbers = flagsContain "-n"
      MatchCaseInsensitive = flagsContain "-i"
      MatchEntireLine = flagsContain "-x"
      MatchInverse = flagsContain "-v" }

let getRegex flags pattern =
    let matchPattern = if flags.MatchEntireLine then "^" + pattern + "$" else pattern
    if flags.MatchCaseInsensitive then
        Regex (matchPattern, RegexOptions.IgnoreCase)
    else
        Regex matchPattern

let grep files flagArguments pattern =
    let flags = parseFlags flagArguments
    let allFiles = Directory.GetFiles(".") |> List.ofSeq |> List.map FileInfo
    let matchingFiles = allFiles |> List.filter (fun f -> files |> List.contains f.Name)
    let allLines =
        matchingFiles
        |> List.collect (fun f ->
            File.ReadAllLines f.FullName
            |> Array.mapi (fun index line -> f.Name, string (index + 1), line)
            |> Array.toList)
    let matchingLines =
        allLines
        |> List.filter (fun (_, _, line) -> (getRegex flags pattern).IsMatch line <> flags.MatchInverse)

    let multipleFiles = List.length files > 1

    let formatOutput (fileName, lineNumber, line) =
        if flags.OutputFileNamesOnly then
            fileName
        else
            let fileNamePrefix = if multipleFiles then fileName + ":" else ""
            fileNamePrefix + (if flags.OutputLineNumbers then lineNumber + ":" + line else line)

    let formattedOutput = matchingLines |> List.map formatOutput

    if flags.OutputFileNamesOnly then
        List.distinct formattedOutput
    else
        formattedOutput