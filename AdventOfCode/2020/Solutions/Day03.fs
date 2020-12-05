module Year2020Day03

open AdventOfCode.Common

[<NoComparison>]
type TreeMap = {
    Height: int
    Width: int
    Trees: char seq
}

type Slope = {
    X: int
    Y: int
}

let isTree (currentPos: (int * int)) (map: char [] list) =
    let (col, row) = currentPos
    let mappedCol = col % map.[row].Length
    map.[row].[mappedCol] = '#'

let parseTreeMap input =
    input
    |> Seq.fold (fun x y -> x @ y) []

let parseLine (line : string) =
    asCharArray line

let nextPosition (currentPos: int * int) (slope: int * int) =
    let col = fst currentPos
    let row = snd currentPos
    let right = fst slope
    let down = snd slope
    
    (col  + right, row + down)
    

let parse = parseEachLine parseLine


let atBottomOfMap currentPos (map: char [] list) =
    match currentPos with
    | (_, mapLength) when mapLength >= map.Length -> true
    | _ -> false
    
    
let findTreeCount slope map =
    let mutable currentPos = (0, 0)
    
    let mutable treeCount = 0
    
    while not (atBottomOfMap currentPos map) do
        
        if isTree currentPos map then
            treeCount <- treeCount + 1
        
        currentPos <- nextPosition currentPos slope
        
    uint64(treeCount)

let solvePart1 (input: seq<char []>) =
    let treeMap =
        input
        |> Seq.toList
     
    findTreeCount (3, 1) treeMap
                   
    

let solvePart2 (input) =
    let treeMap =
        input
        |> Seq.toList
    
    let slopes = [
        (1, 1)
        (3, 1)
        (5, 1)
        (7, 1)
        (1, 2)
    ]
    
    
    slopes
    |> List.map (fun slope -> findTreeCount slope treeMap)
    |> List.reduce (fun product count -> product * count)
    
        
        

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 