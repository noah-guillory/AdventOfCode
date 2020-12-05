module Year2018Day03

open AdventOfCode.Common

type Claim = {
    Id: int
    X: int
    Y: int
    Width: int
    Height: int
}

let overlaps a b =
    a.X < b.X + b.Width && a.X + a.Width > b.X && a.Y < b.Y + b.Height && a.Y + a.Height > b.Y;

    

let parseClaim (line : string) =
    let (claimId, coordinates, dimensions) =
        splitBy " " (fun parts -> ((extractInts parts.[0]), (extractInts parts.[2]), (extractInts parts.[3]))) line
    
    { Id = claimId.[0]; X = coordinates.[0]; Y = coordinates.[1]; Width = dimensions.[0]; Height = dimensions.[1] }
    
    
   
    

let parse = parseEachLine parseClaim

let solvePart1 (claims: Claim seq) =
    claims

let solvePart2 (claims: Claim seq) = 
    claims

let solver = { parse = parse; part1 = solvePart1; part2 = solvePart2 } 