module Year2020Day10

open AdventOfCode.Common
open FSharpx.Collections

type AdapterArray = {
    CurrentOutVoltage: int
    OneVoltDiffs: int
    ThreeVoltDiffs: int
}

let getBestAdapter (adapters: int list) (currentVoltage: int) =
     adapters
        |> List.filter (fun adapter -> (adapter >= currentVoltage + 1) && (adapter <= currentVoltage + 3))
        |> List.min
    

let rec getVoltageDiffs (adapters: int list) (currentVoltage: int) (diffs: Map<int, int>) =
    match adapters with
    | [] ->
         match diffs.TryFind(3) with
            | None -> diffs.Add(3, 1)
            | Some count -> diffs.Add(3, count + 1)
    | adapters ->
        let bestAdapter = getBestAdapter adapters currentVoltage
        
        let diff = bestAdapter - currentVoltage
        
        let updatedDiffs =
            match diffs.TryFind(diff) with
            | None -> diffs.Add(diff, 1)
            | Some count -> diffs.Add(diff, count + 1)
        
        let updatedList =
            adapters
            |> List.except [bestAdapter]
        
        let updatedVoltage = bestAdapter
        
        getVoltageDiffs updatedList updatedVoltage updatedDiffs
        
        

let parseLine (line : string) =
    asInt line

let parse = parseEachLine parseLine

let solvePart1 (input) =
    let diffs =
        getVoltageDiffs (input |> Seq.toList |> List.sort) 0 Map.empty
    
    diffs.[1] * diffs.[3]
    
    
    

let solvePart2 (input) = 
    input

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 