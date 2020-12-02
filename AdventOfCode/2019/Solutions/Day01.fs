module Year2019Day01

open AdventOfCode.Common

let parseLine (line: string) = asInt line

let parse = parseEachLine parseLine

let requiredFuel mass = mass / 3 - 2

let rec getModuleFuel mass =
    match mass with
    | mass when requiredFuel mass < 0 -> 0
    | 0 -> 0
    | mass ->
        requiredFuel mass
        + getModuleFuel (requiredFuel mass)


let solvePart1 (input: int seq) = input |> Seq.map requiredFuel |> Seq.sum

let solvePart2 (input: int seq) = input |> Seq.map getModuleFuel |> Seq.sum

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
