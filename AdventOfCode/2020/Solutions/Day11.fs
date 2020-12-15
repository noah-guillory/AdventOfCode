module Year2020Day11

open AdventOfCode.Common
open FSharpx.Collections

type CellType =
    | Empty
    | Occupied
    | Floor

type NeighborPosition = (int * int)

type Cell = { Type: CellType; Neighbors: int list }

let parseCellType cellChar =
    match cellChar with
    | 'L' -> Empty
    | '#' -> Occupied
    | '.' -> Floor
    | _ -> failwith "Invalid cell"

let getCellChar cell =
    match cell.Type with
    | Empty -> 'L'
    | Occupied -> '#'
    | Floor -> '.'


let getCell (row: int) (col: int) (cells: char [] []) = cells.[row].[col]

let howManyNeighborsOccupied (cell: Cell) (cells: Cell []) =
    cell.Neighbors
    |> List.map (fun position ->
        match cells.[position].Type with
        | Occupied -> true
        | _ -> false)
    |> List.filter id
    |> List.length

let tryFindSeat (position: int * int) (seats: char [] []): NeighborPosition option =
    let (row, col) = position

    match Array.tryItem row seats with
    | None -> None
    | Some foundRow ->
        match Array.tryItem col foundRow with
        | None -> None
        | Some _ -> Some(row, col)

let getNeighbors (row: int) (col: int) (seats: char [] []) =
    let neighborPositions =
        [ (row, col + 1)
          (row, col - 1)
          (row + 1, col)
          (row - 1, col)
          (row + 1, col - 1)
          (row + 1, col + 1)
          (row - 1, col - 1)
          (row - 1, col + 1) ]

    neighborPositions
    |> List.choose (fun position -> seats |> tryFindSeat position)

//let visibleSeats (row: int) (col: int) (seats: char [] []) =
//    let height = seats.Length - 1
//    let width = seats.[0].Length - 1
//
//    let upVisible (row: int) (col: int) =
//        [ for a in row .. -1 .. 0 do
//            if row > 0 then (a, col) ]
//
//
//    let downVisible (row: int) (col: int) =
//        [ for a in row .. height do if a > row then (a, col) ]
//
//
//    let leftVisible (row: int) (col: int) =
//        [ for a in col .. -1 .. 0 do
//            if col > 0 && a <> col then (row, a) ]
//
//
//    let rightVisible (row: int) (col: int) =
//        [ for a in col .. width do if a > col then (row, a) ]
//
//
//    let topLeftVisible (row: int) (col: int) =
//        if row > 0 && col > 0 then
//            [ for c in col .. -1 .. 0 do
//                for r in row .. -1 .. 0 -> (r, c)]
//        else []
//                
//        
//        
//
//
//    let topRightVisible (row: int) (col: int) =
//        if row > 0 then
//            [ for c in 0 .. col do
//                for r in row .. -1 .. 0 -> (r, c) ]
//        else []
//        
//
//
//    let bottomLeftVisible (row: int) (col: int) =
//        if col > 0 then
//            for r in row..col do
//                for c in col .. -1 .. row do
//                    (row, col)
//        else []
//                    
//
//
//
//    let bottomRightVisible (row: int) (col: int) =
//        [ for c in col .. width do
//            for r in row .. height -> (r, c) ]
//
//
//    let generators =
//        [ upVisible
//          downVisible
//          leftVisible
//          rightVisible
//          topLeftVisible
//          topRightVisible
//          bottomLeftVisible
//          bottomRightVisible ]
//
//    generators
//    |> List.fold (fun results generator ->
//        let result = generator row col
//
//        results @ result) List.empty<(int * int)>
//    |> List.choose (fun position -> seats |> tryFindSeat position)




let cycleSeats (cells: Cell []) =
    let cellsCopy = cells |> Array.copy
    for i = 0 to cellsCopy.Length - 1 do
        let currentCell = cellsCopy.[i]

        let occupiedNeighbors =
            howManyNeighborsOccupied currentCell cells

        let nextCellState =
            match currentCell.Type with
            | Occupied -> if occupiedNeighbors >= 4 then { currentCell with Type = Empty } else currentCell
            | Empty -> if occupiedNeighbors = 0 then { currentCell with Type = Occupied } else currentCell
            | Floor -> currentCell
        cellsCopy.[i] <- nextCellState

    cellsCopy

let visibleCycleSeats (cells: Cell []) =
    let cellsCopy = cells |> Array.copy
    for i = 0 to cellsCopy.Length - 1 do
        let currentCell = cellsCopy.[i]

        let occupiedNeighbors =
            howManyNeighborsOccupied currentCell cells

        let nextCellState =
            match currentCell.Type with
            | Occupied -> if occupiedNeighbors >= 5 then { currentCell with Type = Empty } else currentCell
            | Empty -> if occupiedNeighbors = 0 then { currentCell with Type = Occupied } else currentCell
            | Floor -> currentCell
        cellsCopy.[i] <- nextCellState

    cellsCopy

let printSeats rowWidth (cells: Cell []) =
    let rows = cells |> Array.chunkBySize rowWidth

    for row in rows do
        for cell in row do
            printf "%c" (getCellChar cell)

        printf "\n"

    printf "\n"

let parseLine (line: string) = asCharArray line

let parse = parseEachLine parseLine

let solvePart1 (input: seq<char []>) =
    let seats = input |> Seq.toArray

    let rowWidth = seats.[0].Length

    let cells =
        input
        |> Seq.mapi (fun i row -> (i, row))
        |> Seq.fold (fun cells row ->
            let (rowIndex, actualRow) = row

            let newCells =
                actualRow
                |> Array.mapi (fun colIndex cell ->

                    match parseCellType cell with
                    | Empty
                    | Occupied ->
                        let neighbors =
                            getNeighbors rowIndex colIndex seats
                            |> List.map (fun (row, col) -> rowWidth * row + col)

                        { Type = parseCellType cell
                          Neighbors = neighbors }
                    | Floor -> { Type = Floor; Neighbors = [] }




                    )

            Array.append cells newCells) Array.empty<Cell>

    let mutable isDifferent = true
    let mutable lastState = cells

    while isDifferent do
        let nextState = cycleSeats lastState
        //        printSeats rowWidth nextState
        if nextState = lastState then isDifferent <- false else lastState <- nextState

    lastState
    |> Array.filter (fun cell -> cell.Type = Occupied)
    |> Array.length



let solvePart2 (input: seq<char []>) =
    input
//    let seats = input |> Seq.toArray
//
//    let rowWidth = seats.[0].Length
//
//    let cells =
//        input
//        |> Seq.mapi (fun i row -> (i, row))
//        |> Seq.fold (fun cells row ->
//            let (rowIndex, actualRow) = row
//
//            let newCells =
//                actualRow
//                |> Array.mapi (fun colIndex cell ->
//
//                    match parseCellType cell with
//                    | Empty
//                    | Occupied ->
//                        let neighbors =
//                            visibleSeats rowIndex colIndex seats
//                            |> List.map (fun (row, col) -> rowWidth * row + col)
//
//                        { Type = parseCellType cell
//                          Neighbors = neighbors }
//                    | Floor -> { Type = Floor; Neighbors = [] })
//
//            Array.append cells newCells) Array.empty<Cell>
//
//    let mutable isDifferent = true
//    let mutable lastState = cells
//
//    while isDifferent do
//        let nextState = visibleCycleSeats lastState
//        printSeats rowWidth nextState
//        if nextState = lastState then isDifferent <- false else lastState <- nextState
//
//    lastState
//    |> Array.filter (fun cell -> cell.Type = Occupied)
//    |> Array.length

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
