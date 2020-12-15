module Year2020Day12

open System
open AdventOfCode.Common

type Instruction<'T> =
    | MoveNorth of 'T
    | MoveEast of 'T
    | MoveSouth of 'T
    | MoveWest of 'T
    | MoveForward of 'T
    | TurnLeft of 'T
    | TurnRight of 'T

type Position = { EastWest: int; NorthSouth: int }

type Ship = { Heading: int; Position: Position }

type Waypoint = { Position: Position }


let movePositionNorth (position: Position) (amount: int) =
    { position with
          NorthSouth = position.NorthSouth + amount }

let movePositionSouth (position: Position) (amount: int) =
    { position with
          NorthSouth = position.NorthSouth - amount }

let movePositionEast (position: Position) (amount: int) =
    { position with
          EastWest = position.EastWest + amount }

let movePositionWest (position: Position) (amount: int) =
    { position with
          EastWest = position.EastWest - amount }

let turnShipLeft (ship: Ship) (amount: int) =
    { ship with
          Heading = (ship.Heading - amount) % 360 }

let turnShipRight (ship: Ship) (amount: int) =
    { ship with
          Heading = (ship.Heading + amount) % 360 }

let rotateWaypointLeft (waypoint: Waypoint) degrees =
    match degrees with
    | 90 ->
        { waypoint with
              Position =
                  { EastWest = -waypoint.Position.NorthSouth
                    NorthSouth = waypoint.Position.EastWest } }
    | 180 ->
        { waypoint with
              Position =
                  { EastWest = -waypoint.Position.EastWest
                    NorthSouth = -waypoint.Position.NorthSouth } }
    | 270 ->
        { waypoint with
              Position =
                  { EastWest = waypoint.Position.NorthSouth
                    NorthSouth = -waypoint.Position.EastWest } }
    | _ -> failwith "Invalid rotation"

let rotateWaypointRight (waypoint: Waypoint) degrees =
    match degrees with
    | 90 ->
        { waypoint with
              Position =
                  { EastWest = waypoint.Position.NorthSouth
                    NorthSouth = -waypoint.Position.EastWest } }
    | 180 ->
        { waypoint with
              Position =
                  { EastWest = -waypoint.Position.EastWest
                    NorthSouth = -waypoint.Position.NorthSouth } }
    | 270 ->
        { waypoint with
              Position =
                  { EastWest = -waypoint.Position.NorthSouth
                    NorthSouth = waypoint.Position.EastWest } }
    | _ -> failwith "Invalid rotation"


let followInstruction (ship: Ship) (instruction: Instruction<int>): Ship =
    match instruction with
    | TurnLeft degrees -> turnShipLeft ship degrees
    | TurnRight degrees -> turnShipRight ship degrees
    | MoveForward distance ->
        match ship.Heading with
        | 360
        | 0 ->
            { ship with
                  Position = movePositionNorth ship.Position distance }
        | -270
        | 90 ->
            { ship with
                  Position = movePositionSouth ship.Position distance }
        | -180
        | 180 ->
            { ship with
                  Position = movePositionEast ship.Position distance }
        | -90
        | 270 ->
            { ship with
                  Position = movePositionWest ship.Position distance }
        | _ -> failwith "Invalid heading"
    | MoveNorth distance ->
        { ship with
              Position = movePositionNorth ship.Position distance }
    | MoveSouth distance ->
        { ship with
              Position = movePositionSouth ship.Position distance }
    | MoveEast distance ->
        { ship with
              Position = movePositionEast ship.Position distance }
    | MoveWest distance ->
        { ship with
              Position = movePositionWest ship.Position distance }


let followInstructionRelative (pair: (Ship * Waypoint)) (instruction: Instruction<int>): (Ship * Waypoint) =
    let (ship, waypoint) = pair

    match instruction with
    | TurnLeft degrees -> (ship, rotateWaypointLeft waypoint degrees)
    | TurnRight degrees -> (ship, rotateWaypointRight waypoint degrees)
    | MoveForward multiplier ->
        let (northSouthDistance, eastWestDistance) =
            (multiplier * waypoint.Position.NorthSouth, multiplier * waypoint.Position.EastWest)

        ({ ship with
               Position =
                   { NorthSouth = ship.Position.NorthSouth + northSouthDistance
                     EastWest = ship.Position.EastWest + eastWestDistance } },
         waypoint)
    | MoveNorth distance ->
        (ship,
         { waypoint with
               Position = movePositionNorth waypoint.Position distance })
    | MoveSouth distance ->
        (ship,
         { waypoint with
               Position = movePositionSouth waypoint.Position distance })
    | MoveEast distance ->
        (ship,
         { waypoint with
               Position = movePositionEast waypoint.Position distance })
    | MoveWest distance ->
        (ship,
         { waypoint with
               Position = movePositionWest waypoint.Position distance })


let parseLine (line: string) =
    let value = line.[1..] |> int

    match line.[0] with
    | 'N' -> MoveNorth value
    | 'E' -> MoveEast value
    | 'S' -> MoveSouth value
    | 'W' -> MoveWest value
    | 'F' -> MoveForward value
    | 'L' -> TurnLeft value
    | 'R' -> TurnRight value
    | _ -> failwith "Invalid action"





let parse = parseEachLine parseLine

let solvePart1 (input) =
    let startingShip =
        { Heading = 90
          Position = { NorthSouth = 0; EastWest = 0 } }

    let endingState =
        input |> Seq.fold followInstruction startingShip

    Math.Abs(endingState.Position.EastWest)
    + Math.Abs(endingState.Position.NorthSouth)


let solvePart2 (input) =
    let startingShip =
        { Heading = 90
          Position = { NorthSouth = 0; EastWest = 0 } }

    let startingWaypoint =
        { Position = { EastWest = 10; NorthSouth = 1 } }

    let pair = (startingShip, startingWaypoint)

    let (endingShip, endingWaypoint) =
        input |> Seq.fold followInstructionRelative pair



    Math.Abs(endingShip.Position.EastWest)
    + Math.Abs(endingShip.Position.NorthSouth)

let solver =
    { parse = parse
      part1 = solvePart1
      part2 = solvePart2 }
