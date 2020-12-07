module Year2020Day06

open AdventOfCode.Common

open System
open FSharpx.Collections

let parseLine (line: string) =
    match line with
    | "" -> "|"
    | _ -> asString line

let parse = parseEachLine parseLine

let getGroupAnswers (groupAnswers: string) =
    groupAnswers.Split " "
    |> Array.map Seq.toList
    |> List.concat
    |> List.fold (fun (answerMap: Map<char, int>) (answer: char) ->
        match answerMap.TryFind(answer) with
        | Some _ -> answerMap
        | None -> answerMap.Add(answer, 1)) Map.empty

let getGroupAnswersPart2 (groupAnswers: string) =
    let groupSize = groupAnswers.Split(" ").Length

    groupAnswers.Split " "
    |> Array.map Seq.toList
    |> List.concat
    |> List.fold (fun (answerMap: Map<char, int>) (answer: char) ->
        match answerMap.TryFind(answer) with
        | Some count -> answerMap.Add(answer, count + 1)
        | None -> answerMap.Add(answer, 1)) Map.empty
    |> Map.filter (fun _ value -> value = groupSize)

let solvePart1 (input: string seq) =
    let data = input |> Seq.toArray

    let answers =
        String.Join(' ', data) |> splitBy " | " id

    answers
    |> Array.map getGroupAnswers
    |> Array.fold (fun sum groupMap ->
        let allYes = groupMap |> Map.values |> Seq.sum
        sum + allYes) 0

let solvePart2 (input: string seq) =
    let data = input |> Seq.toArray

    let answers =
        String.Join(' ', data) |> splitBy " | " id

    answers
    |> Array.map getGroupAnswersPart2
    |> Array.fold (fun sum groupMap ->
        let allYes = groupMap |> Map.keys |> Seq.length
        sum + allYes) 0

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
