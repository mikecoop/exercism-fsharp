module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    let nucleotides = [ 'A'; 'C'; 'G'; 'T' ]
    let empty = nucleotides |> List.map (fun c -> c, 0) |> Map.ofList
    let countChar (map:Map<char, int>) (c:char) : Map<char, int> =
        if map.ContainsKey(c) then
            map.Add(c, map.[c] + 1)
        else
            map.Add(c, 1)
    let countMap = strand |> Seq.fold countChar empty
    let foundNucleotides = countMap |> Map.toList |> List.map fst
    if nucleotides = foundNucleotides then
        Some countMap
    else
        None