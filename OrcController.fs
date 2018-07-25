module OrcController

open Model
open View

let walkSpeed = 0.05

let checkForFloor (nx, y) direction blocks =
    let testy = int y + 1
    let testx = if direction = Left then floor nx |> int else ceil nx |> int
    blocks |> List.exists (fun (bx, by, _) -> (bx, by) = (testx, testy))

let processOrc runState worldState (orc : Orc) =
    let x, y = orc.position
    let nx = if orc.direction = Left then x - walkSpeed else x + walkSpeed
    let isEdge = checkForFloor (nx, y) orc.direction worldState.blocks |> not

    let direction = if not isEdge then orc.direction else if orc.direction = Left then Right else Left
    let position = if not isEdge then (nx, y) else orc.position
    
    { orc  with 
        direction = direction
        position = position
        state = Walking }

let processOrcs runState worldState =
    { worldState with orcs = worldState.orcs |> List.map (processOrc runState worldState) }