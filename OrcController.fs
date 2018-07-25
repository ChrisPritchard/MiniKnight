module OrcController

open Model

let walkSpeed = 0.05

let checkForFloor (nx, y) direction blocks =
    let testy = int y + 1
    let testx = if direction = Left then floor nx |> int else ceil nx |> int
    blocks |> List.exists (fun (bx, by, _) -> (bx, by) = (testx, testy))

let checkForFriend (nx, y) direction = 
    List.exists (fun (ox, oy) -> 
        oy = y &&
        (match direction with
        | Left -> ox > nx - 1. && ox < nx
        | Right -> nx < ox && nx + 1. > ox))

let processOrc runState worldState (orc : Orc) =
    let x, y = orc.position
    let nx = if orc.direction = Left then x - walkSpeed else x + walkSpeed

    let otherOrcs = worldState.orcs |> List.except [orc] |> List.map (fun o -> o.position)
    let shouldTurn = 
        checkForFloor (nx, y) orc.direction worldState.blocks |> not
        || checkForFriend (nx, y) orc.direction otherOrcs

    let direction = if not shouldTurn then orc.direction else if orc.direction = Left then Right else Left
    let position = if not shouldTurn then (nx, y) else orc.position
    
    { orc  with 
        direction = direction
        position = position
        state = Walking }

let processOrcs runState worldState =
    { worldState with orcs = worldState.orcs |> List.map (processOrc runState worldState) }