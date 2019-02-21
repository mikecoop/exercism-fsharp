module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    let students = ((school.TryFind grade) |> Option.defaultValue [ ]) @ [ student ] |> List.sort
    school.Add(grade, students)

let roster (school: School): string list = school |> Map.toList |> List.map snd |> List.collect id

let grade (number: int) (school: School): string list = school.TryFind number |> defaultArg <| [ ]
