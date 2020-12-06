module Year2020Day05

open AdventOfCode.Common

type BoardingPass = {
    RowSegment: string
    ColumnSegment: string
}

type Seat = {
    Row: int
    Column: int
    ID: int
}

let splitList s =
        let len = List.length s in
        s |> List.mapi (fun i x -> (i<len/2,x))
          |> List.partition fst
          |> (fun (x,y) -> ((x |> List.map snd),(y |> List.map snd)))

type Tree<'a> = 
  | Empty
  | Node of value: 'a * left: Tree<'a> * right: Tree<'a>
  
  
let bisectRows command rows =
    match command with
    | 'F'
    | 'L' -> fst (splitList rows)
    | 'B'
    | 'R' -> snd (splitList rows)
    | _ -> rows
  
let determineRow (boardingPass: BoardingPass): int =
    let mutable rows = [0..127]
    
    for command in boardingPass.RowSegment do
        rows <- bisectRows command rows
    
    rows.[0]
    
let determineColumn (boardingPass: BoardingPass): int =
    let mutable rows = [0..7]
    
    for command in boardingPass.ColumnSegment do
        rows <- bisectRows command rows
    
    rows.[0]
    
let getSeat (boardingPass: BoardingPass): (int * int) =
    (determineRow boardingPass, determineColumn boardingPass)
    
let createSeat (boardingPass: BoardingPass): Seat =
    let row, column = getSeat boardingPass
    { Row = row; Column = column; ID = row * 8 + column }
    
    

let parseLine (line : string) =
    let parsedLine = asString line
    { RowSegment = parsedLine.[..6]
      ColumnSegment = parsedLine.[parsedLine.Length-3..] }

let parse = parseEachLine parseLine

let solvePart1 (input) =
    input
    |> Seq.toList
    |> List.map createSeat
    |> List.map (fun seat -> seat.ID)
    |> List.max
    
let solvePart2 (input) = 
    let seatIds =
        input
        |> Seq.toList
        |> List.map createSeat
        |> List.map (fun seat -> seat.ID)
        
    let min = seatIds |> List.min
    let max = seatIds |> List.max
    
    let possibles = [min..max]
    
    set possibles - set seatIds
    
    
    
    
    
    
    

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 