module Year2020Day08

open AdventOfCode.Common
open FSharpx.Collections

type Instruction<'T> =
    | Acc of 'T
    | Jmp of 'T
    | Nop of 'T

let parseInstruction instruction =
    let (instruction, arg) = splitBy " " (fun p -> (p.[0], int(p.[1]))) instruction
    match instruction with
    | "acc" -> Acc arg
    | "jmp" -> Jmp arg
    | "nop" -> Nop arg
    | code -> failwithf "Invalid code %s" code
    
let runProgramTilEnd (program: Instruction<int> array) =
    let mutable pc = 0
    let mutable acc = 0
    let mutable insRun = Array.create program.Length false
    
    let continueProgram pc (instructions: Instruction<int> array) (ranInstructions: bool array) =
        if pc > instructions.Length - 1 then false
        elif ranInstructions.[pc] = true then false
        else true
    
        
    while continueProgram pc program insRun do
        let curIns = program.[pc]
        insRun.[pc] <- true
        match curIns with
        | Acc accum ->
            acc <- acc + accum
            pc <- pc + 1
        | Jmp jumpTo -> pc <- pc + jumpTo
        | Nop _ -> pc <- pc + 1
        
        
    if pc > program.Length - 1 then (acc, true)
    else (acc, false)
     


let parseLine (line : string) =
    asString line

let parse = parseEachLine parseLine

let solvePart1 (input) =
    let program =
        input
        |> Seq.toArray
        |> Array.map parseInstruction
    
    let result = program |> runProgramTilEnd
    
    fst result

let solvePart2 (input) = 
    let instructions =
        input
        |> Seq.toArray
        |> Array.map parseInstruction
        
        
    let potentialCorruptedInstructions =
        instructions
        |> Array.mapi (fun index ins -> (ins, index))
        |> Array.filter (fun ins ->
            match fst ins with
            | Jmp _
            | Nop _ -> true
            | _ -> false
            )
    
    let mutable correctAcc = (0, false)
    
    for potential in potentialCorruptedInstructions do
        let change =
            match fst potential with
            | Jmp arg -> Nop arg
            | Nop arg -> Jmp arg
            | _ -> failwith "Invalid change."
        
        
        let mutable newProgram = instructions |> Array.copy
        newProgram.[snd potential] <- change
        
        let result = newProgram |> runProgramTilEnd
        
        if snd result then correctAcc <- result
        
    correctAcc
        
        
    
    
    

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 