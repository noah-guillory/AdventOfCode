module Year2020Day09

open AdventOfCode.Common
open FSharpx.Collections

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let parseLine (line : string) =
    asInt64 line

let parse = parseEachLine parseLine


let lastNumberValid (segment: int64 list) =
   let sums =
       segment
       |> comb 2
       |> List.map (fun pair -> pair.[0] + pair.[1])
   
   let lastNumber = segment |> List.last
   
   sums |> List.contains lastNumber
   

let solvePart1 (input: int64 seq) =
    input
    |> Seq.toList
    |> List.windowed 26
    |> List.filter (fun w -> lastNumberValid w <> true)
    |> List.head
    |> List.last        
    

    
    

let solvePart2 (input) = 
    input

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 