open System.Text.RegularExpressions
#I __SOURCE_DIRECTORY__

open System.IO

let grep files flagArguments pattern =
    let patternRegex = Regex pattern
    let foundFiles = Directory.GetFiles("grep\\bin\\debug\\netcoreapp2.1") |> Seq.map FileInfo
    let matchingFiles = foundFiles |> Seq.filter (fun f -> files |> List.contains f.Name)
    let allLines =
        matchingFiles
        |> Seq.collect (fun f ->
            File.ReadAllLines f.FullName
            |> Array.mapi (fun index line -> f.Name, index, line)
            |> Array.toSeq)
    let matchingLines =
        allLines
        |> Seq.filter (fun (_, _, line) -> patternRegex.IsMatch line)
    matchingLines
    |> Seq.map (fun (_, _, line) -> line)
    |> Seq.toList

grep ["iliad.txt"; "paradise-lost.txt"] [ ] "Agamemnon"