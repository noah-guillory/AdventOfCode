namespace AdventOfCode.Y2019

open AdventOfCode
open AdventOfCode.Common
open AdventOfCode.Benchmarking

module Program =
    let getSolver day part printAnswer =
        let run (solver : Day<_, _, _>) =
            Runner.run printAnswer 2019 day part solver
        match day with
        | 1  -> run Year2019Day01.solver | 2 -> run Year2019Day02.solver
        | day -> fun _ -> printfn "Invalid Day: %i (Year %i)" day 2019

    type Bench2019() =
        inherit Bench() with
            override _.GetSolverFunc day part () =
                getSolver day part false ()

    [<EntryPoint>]
    let main argv =
        let runPart day part = getSolver day part true ()
        let runDay day = for part in 1..2 do runPart day part
        match argv.[0] with
            | "BENCH" -> Benchmarking.runBenchmarks<Bench2019>()
            | "ALL" -> for day in 1..2 do runDay day
            | x ->
                let parts = x.Split('.') |> Array.map int
                match parts.Length with
                | 1 -> runDay parts.[0]
                | 2 -> runPart parts.[0] parts.[1]
                | _ -> ()
        0