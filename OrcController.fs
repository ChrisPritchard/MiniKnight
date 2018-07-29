module OrcController

open GameCore
open Model
open View

let walkSpeed = 0.05
let guardTime = 1000.
let readyTime = 500.

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

let processOrc (runState : RunState) worldState knight (orc : Orc) =
    let isInAttackRange = checkForEnemy orc.position orc.direction worldState.knight.position

    match orc.state with
    | Falling t when runState.elapsed - t > (animSpeed * float dyingFrames) ->
        { orc with state = Slain }, knight, []
    | Guarding t when runState.elapsed - t > guardTime ->
        { orc with state = if isInAttackRange then ReadyingAttack runState.elapsed else Patrolling }, knight, []
    | ReadyingAttack t when runState.elapsed - t > readyTime ->
        let newKnight = 
            if isInAttackRange && knight.state <> Blocking then
                { knight with 
                    health = knight.health - 1
                    state = if knight.health = 1 then Dying runState.elapsed else knight.state }
            else knight
        let newEvents = 
            if isInAttackRange then
                match newKnight.state with
                | Blocking -> [OrcSwing;KnightBlocked]
                | Dying _ -> [OrcSwing;KnightDying]
                | _ -> [OrcSwing;KnightHit]
            else []
        { orc with state = if isInAttackRange then Attacking runState.elapsed else Patrolling }, newKnight, newEvents
    | Attacking t when runState.elapsed - t > (animSpeed * float strikeFrames) ->
        { orc with state = Patrolling }, knight, []
    | Falling _ 
    | ReadyingAttack _ | Guarding _ | Attacking _ | Slain -> orc, knight, []
    | _ when isInAttackRange ->
        { orc with state = Guarding runState.elapsed }, knight, []
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
            state = Patrolling }, knight, []

let processOrcs runState worldState =
    let knight, orcs, events = List.fold (fun (k,ol,ev) o -> 
        let newOrc, knight, newEvents = processOrc runState worldState k o
        (knight, newOrc::ol, newEvents @ ev)) (worldState.knight, [], []) worldState.orcs
    { worldState with 
        knight = knight
        orcs = orcs
        events = events @ worldState.events }