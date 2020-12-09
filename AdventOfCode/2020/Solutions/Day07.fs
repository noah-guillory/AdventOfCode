module Year2020Day07

open AdventOfCode.Common
open System
open FSharpx.Collections


type Rule = (int * string)
type BagMap = Map<string, Map<string, int>>



let identifyBag (rule: string) = String.Join(" ", (rule.Split " ").[..1])

let breakUpSegments segments =
    segments
    |> Array.fold (fun (ruleMap: Map<string, int>) (segment: string) ->
        ruleMap.Add(String.Join(" ", (segment.Split " ").[1..2]), (extractInts segment).[0])) Map.empty

let getRules (rule: string) =
    let ruleSegment = rule.Split("contain ").[1]

    if ruleSegment <> "no other bags" then
        match ruleSegment.Contains(",") with
        | true -> breakUpSegments (ruleSegment.Split(", "))
        | false -> breakUpSegments [| ruleSegment |]
    else
        Map.empty





let parseLine (line: string) = asString line.[..line.Length - 2]


let rec containsShinyGold (bagName: string) (bagMap: BagMap) =
    Map.keys (bagMap.[bagName])
    |> Seq.exists (fun child ->
        if child = "shiny gold" then true
        elif containsShinyGold child bagMap then true
        else false)


let parse = parseEachLine parseLine

let solvePart1 (input) =
    let allRules = input |> Seq.toArray

    let bagNames = allRules |> Array.map identifyBag

    let rules = allRules |> Array.map getRules


    let compiledRules =
        Array.fold2 (fun (bagMap: BagMap) (name: string) (rules: Map<string, int>) -> bagMap.Add(name, rules)) Map.empty
            bagNames rules

    bagNames
    |> Array.fold (fun containsList bagName ->
        if containsShinyGold bagName compiledRules then bagName :: containsList else containsList) List.empty
    |> List.length


let solvePart2 (input) = input

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
