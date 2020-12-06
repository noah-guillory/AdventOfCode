module Year2020Day04

open System.Text.RegularExpressions
open AdventOfCode.Common
open System

type PassportDataPoint =
    | NumericData of int
    | TextData of string

type Passport =
    { BirthYear: PassportDataPoint option
      IssueYear: PassportDataPoint option
      ExpirationYear: PassportDataPoint option
      Height: PassportDataPoint option
      HairColor: PassportDataPoint option
      EyeColor: PassportDataPoint option
      PassportID: PassportDataPoint option
      CountryID: PassportDataPoint option }





let parseLine (line: string) =
    match line with
    | "" -> "|"
    | _ -> asString line

let parsePassport rawData = rawData |> splitBy " "

let getDataPoint (point: string): (string * PassportDataPoint) =
    let getIntData point = (extractInts point).[0]
    let getStringData (point: string) = point.Split(':').[1]


    match point.[..2] with
    | "iyr"
    | "eyr"
    | "byr" as dataType -> dataType, NumericData(getIntData point)
    | "hgt"
    | "hcl"
    | "ecl"
    | "pid"
    | "cid" as dataType -> dataType, TextData(getStringData point)
    | unknownType -> failwithf "Incompatible data type %s" unknownType



let getIntData dataPoint =
    match dataPoint with
    | NumericData data -> data
    | _ -> failwith "Not numeric data"

let getStringData dataPoint =
    match dataPoint with
    | TextData data -> data
    | _ -> failwith "Not text data"

let createPassport (passportData: Map<string, PassportDataPoint>): Passport =
    { BirthYear = passportData.TryFind "byr"
      IssueYear = passportData.TryFind "iyr"
      ExpirationYear = passportData.TryFind "eyr"
      Height = passportData.TryFind "hgt"
      HairColor = passportData.TryFind "hcl"
      EyeColor = passportData.TryFind "ecl"
      PassportID = passportData.TryFind "pid"
      CountryID = passportData.TryFind "cid" }

let parseDataPoints (dataLine: string) =
    dataLine.Split(" ")
    |> Array.map getDataPoint
    |> Map.ofArray


let isValidPassportPart1 (passport: Passport) =
    passport.Height.IsSome
    && passport.BirthYear.IsSome
    && passport.ExpirationYear.IsSome
    && passport.IssueYear.IsSome
    && passport.HairColor.IsSome
    && passport.EyeColor.IsSome
    && passport.PassportID.IsSome

let isValidPassportPart2 (passport: Passport) =
    let birthYearValid =
        (fun passport ->
            (passport.BirthYear.IsSome)
            && passport.BirthYear.Value >= NumericData(1920)
            && passport.BirthYear.Value <= NumericData(2020))

    let issueYearValid =
        (fun passport ->
            (passport.IssueYear.IsSome)
            && passport.IssueYear.Value >= NumericData(2010)
            && passport.IssueYear.Value <= NumericData(2020))

    let expirationYearValid =
        (fun passport ->
            (passport.ExpirationYear.IsSome)
            && passport.ExpirationYear.Value >= NumericData(2020)
            && passport.ExpirationYear.Value <= NumericData(2030))

    let heightValid =
        (fun passport ->
            if passport.Height.IsSome then
                let height = getStringData passport.Height.Value
                let unit = height.[height.Length - 2..]

                if unit = "cm" || unit = "in" then
                    let amount = int (height.[..height.Length - 3])

                    match unit with
                    | "cm" -> amount >= 150 && amount <= 193
                    | "in" -> amount >= 59 && amount <= 76
                    | _ -> false
                else
                    false
            else
                false)

    let hairColorValid =
        (fun passport ->
            if passport.HairColor.IsSome then
                let color = getStringData passport.HairColor.Value
                Regex.IsMatch(color, "#([a-f0-9]{6})$")
            else
                false

            )

    let eyeColorValid =
        (fun passport ->
            if passport.EyeColor.IsSome then
                let color = getStringData passport.EyeColor.Value

                match color with
                | "amb"
                | "blu"
                | "brn"
                | "gry"
                | "grn"
                | "hzl"
                | "oth" -> true
                | _ -> false
            else
                false)

    let passportIdValid =
        (fun passport ->
            if passport.PassportID.IsSome then
                let passportId = getStringData passport.PassportID.Value
                passportId.Length = 9
            else
                false)

    let rules =
        [ birthYearValid
          issueYearValid
          expirationYearValid
          heightValid
          hairColorValid
          eyeColorValid
          passportIdValid ]

    let buildValidator rules =
        rules
        |> List.reduce (fun firstRule secondRule passport ->
            let passed = firstRule passport

            if passed then
                let passed = secondRule passport
                if passed then passed else false
            else
                false)

    let validate = buildValidator rules

    validate passport





let parse = parseEachLine parseLine

let solvePart1 (input: string seq) =
    let data = input |> Seq.toArray

    let passportData =
        String.Join(' ', data) |> splitBy " | " id

    let validPassports =
        passportData
        |> Array.map parseDataPoints
        |> Array.map createPassport
        |> Array.filter isValidPassportPart1


    validPassports.Length





let solvePart2 (input: string seq) =
    let data = input |> Seq.toArray

    let passportData =
        String.Join(' ', data) |> splitBy " | " id

    let validPassports =
        passportData
        |> Array.map parseDataPoints
        |> Array.map createPassport
        |> Array.filter isValidPassportPart2


    validPassports.Length

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
