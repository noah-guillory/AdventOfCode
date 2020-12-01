﻿module Year2020Day01

open NoahGuillory.AdventOfCode.Common

let charToLevelDiff =
    function
    | '(' -> 1
    | ')' -> -1
    | c -> failwithf "Invalid character: %c" c

let solvePart1 input =
    input
    |> Seq.sumBy charToLevelDiff

let solvePart2 input =
    input
    |> Seq.map charToLevelDiff
    |> Seq.scan (+) 0
    |> Seq.findIndex (fun l -> l < 0)

let solver = { parse = parseFirstLine asString; part1 = solvePart1; part2 = solvePart2 }