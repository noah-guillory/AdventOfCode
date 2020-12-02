module Year2020Day01

open NoahGuillory.AdventOfCode.Common

let solvePart1 (input: int seq) =
    let inList = input |> Seq.toList
    
    let res = [ for i in inList do
                for j in inList do
                if i + j = 2020 then i * j ]
        
    
    res.Head
    

let solvePart2 (input: int seq) =
    let inList = input |> Seq.toList
    
    let res = [ for i in inList do
                for j in inList do
                for k in inList do
                if i + j + k = 2020 then i * j * k ]
        
    res.Head
let solver = { parse = parseEachLine asInt; part1 = solvePart1; part2 = solvePart2 }