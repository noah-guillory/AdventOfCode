namespace AdventOfCode.Y2020

open AdventOfCode
open AdventOfCode.Common
open AdventOfCode.Benchmarking

module Program =
    let getSolver day part printAnswer =
        let run (solver : Day<_, _, _>) =
            Runner.run printAnswer 2020 day part solver
        match day with
        | 1  -> run Year2020Day01.solver | 2  -> run Year2020Day02.solver | 3 -> run Year2020Day03.solver
        | day -> fun _ -> printfn "Invalid Day: %i (Year %i)" day 2020

    type Bench2020() =
        inherit Bench() with
            override _.GetSolverFunc day part () =
                getSolver day part false ()

    [<EntryPoint>]
    let main argv =
        let runPart day part = getSolver day part true ()
        let runDay day = for part in 1..2 do runPart day part
        match argv.[0] with
            | "BENCH" -> Benchmarking.runBenchmarks<Bench2020>()
            | "ALL" -> for day in 1..3 do runDay day
            | x ->
                let parts = x.Split('.') |> Array.map int
                match parts.Length with
                | 1 -> runDay parts.[0]
                | 2 -> runPart parts.[0] parts.[1]
                | _ -> ()
        0