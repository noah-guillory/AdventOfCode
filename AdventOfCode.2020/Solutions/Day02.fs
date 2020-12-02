module Year2020Day02

open NoahGuillory.AdventOfCode.Common

type Policy =
    { PolicyChar: char
      Min: int
      Max: int }

type PasswordEntry = { Policy: Policy; Password: string }

let count x = Seq.filter ((=) x) >> Seq.length

let parsePolicy (policy: string) =
    let (limits, char) =
        splitBy " " (fun p -> (p.[0], char (p.[1]))) policy

    let limits = splitBy "-" asIntArray limits

    { PolicyChar = char
      Min = limits.[0]
      Max = limits.[1] }

let parsePasswordEntry (passwordEntry: string list): PasswordEntry =
    let (policy, password) = passwordEntry.[0], passwordEntry.[1]

    { Policy = parsePolicy policy
      Password = password }

let isValidPassword passwordEntry =
    let policyCharCount =
        passwordEntry.Password
        |> count passwordEntry.Policy.PolicyChar

    match policyCharCount with
    | inRange when inRange >= passwordEntry.Policy.Min
                   && inRange <= passwordEntry.Policy.Max -> true
    | _ -> false

let isValidPasswordCorrect passwordEntry =
    (passwordEntry.Password.[passwordEntry.Policy.Min] = passwordEntry.Policy.PolicyChar) <>
    (passwordEntry.Password.[passwordEntry.Policy.Max] = passwordEntry.Policy.PolicyChar)

let solvePart1 passwordEntries =
    passwordEntries
    |> Seq.map parsePasswordEntry
    |> Seq.filter isValidPassword
    |> Seq.length



let solvePart2 passwordEntries =
    passwordEntries
    |> Seq.map parsePasswordEntry
    |> Seq.filter isValidPasswordCorrect
    |> Seq.length


let solver =
    { parse = parseEachLine (splitBy ":" asStringArray >> Array.toList)
      part1 = solvePart1
      part2 = solvePart2 }
