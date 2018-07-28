module OrcController

open GameCore
open Model
open View

let walkSpeed = 0.05
let guardTime = 1000.
let readyTime = 1000.

let checkForFloor (nx, y) direction blocks =
    let testy = int y + 1
    let testx = if direction = Left then floor nx |> int else ceil nx |> int
    blocks |> List.exists (fun (bx, by, _) -> (bx, by) = (testx, testy))


let checkForWall (nx, y) direction = 
    List.exists (fun (bx, by, _) -> 
        let fbx = float bx
        by = int y &&
        (match direction with
        | Left -> fbx > nx - 1. && fbx < nx
        | Right -> nx < fbx && nx + 1. > fbx))

let checkForFriend (nx, y) direction = 
    List.exists (fun (ox, oy) -> 
        oy = y &&
        (match direction with
        | Left -> ox > nx - 1. && ox < nx
        | Right -> nx < ox && nx + 1. > ox))

let checkForEnemy (x, y) direction (kx, ky) = 
    ky = y &&
    (match direction with
    | Left -> kx > x - 1.5 && kx < x
    | Right -> x < kx && x + 1.5 > kx)

let processOrc (runState : RunState) worldState (orc : Orc) =
    let isInAttackRange = checkForEnemy orc.position orc.direction worldState.knight.position
    match orc.state with
    | Falling t when runState.elapsed - t > (animSpeed * float dyingFrames) ->
        { orc with state = Slain }
    | Guarding t when runState.elapsed - t > guardTime ->
        { orc with state = if isInAttackRange then ReadyingAttack runState.elapsed else Patrolling }
    | ReadyingAttack t when runState.elapsed - t > readyTime ->
        // check for player hit
        { orc with state = if isInAttackRange then Attacking runState.elapsed else Patrolling }
    | Attacking t when runState.elapsed - t > (animSpeed * float strikeFrames) ->
        { orc with state = Patrolling }
    | Falling _ 
    | ReadyingAttack _ | Guarding _ | Attacking _ | Slain -> orc
    | _ when isInAttackRange ->
        { orc with state = Guarding runState.elapsed }
    | _ ->
        let x, y = orc.position
        let nx = if orc.direction = Left then x - walkSpeed else x + walkSpeed

        let otherOrcs = worldState.orcs |> List.except [orc] |> List.map (fun o -> o.position)
        let shouldTurn = 
            checkForFloor (nx, y) orc.direction worldState.blocks |> not
            || checkForWall (nx, y) orc.direction worldState.blocks
            || checkForFriend (nx, y) orc.direction otherOrcs

        let direction = if not shouldTurn then orc.direction else if orc.direction = Left then Right else Left
        let position = if not shouldTurn then (nx, y) else orc.position
        
        { orc  with 
            direction = direction
            position = position
            state = Patrolling }

let processOrcs runState worldState =
    { worldState with orcs = worldState.orcs |> List.map (processOrc runState worldState) }