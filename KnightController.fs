module KnightController

open GameCore
open Model
open View
open CollisionDetection
open Microsoft.Xna.Framework.Input

let timeForStrikes = animSpeed * 2.
let timeForDying = animSpeed * 5.

let walkSpeed = 0.15
let jumpSpeed = -0.55
let gravityStrength = 0.05
let terminalVelocity = 0.9

let walkLeftKeys = [Keys.A;Keys.Left]
let walkRightKeys = [Keys.D;Keys.Right]
let jumpKeys = [Keys.W;Keys.Space]
let strikeKeys = [Keys.LeftControl;Keys.RightControl]
let blockKeys = [Keys.LeftAlt;Keys.RightAlt]

let tryApplyVelocity verticalSpeed (x, y) blocks =
    let ny = y + verticalSpeed
    let blocks = blocks |> List.map (fun (bx, by, _) -> (bx, by))
    if verticalSpeed < 0. then
        let ceiling = tryFindCollision North (x, ny) blocks
        match ceiling with
        | Some (_, by) -> (x, float by + 1.), Some 0.
        | None -> (x, ny), Some verticalSpeed
    else
        let floor = tryFindCollision South (x, ny) blocks
        match floor with
        | Some (_, by) -> (x, float by - 1.), None
        | None -> (x, ny), Some verticalSpeed

let tryWalk direction (x, y) blocks =
    let nx = if direction = Left then x - walkSpeed else x + walkSpeed
    let blocks = blocks |> List.map (fun (bx, by, _) -> (bx, by))
    if direction = Left then
        let wall = tryFindCollision West (nx, y) blocks
        match wall with
        | Some (bx, _) -> (float bx + 1., y)
        | None -> (nx, y)
    else
        let wall = tryFindCollision East (nx, y) blocks
        match wall with
        | Some (bx, _) -> (float bx - 1., y)
        | None -> (nx, y)

let getWalkCommand (runState: RunState) =
    let left = if runState.IsAnyPressed walkLeftKeys then Some Left else None
    let right = if runState.IsAnyPressed walkRightKeys then Some Right else None
    match [left;right] |> List.choose id with
    | [dir] -> Some dir
    | _ -> None

let canStrike runState controllerState = 
    runState.elapsed - controllerState.lastStrikeTime >= timeForStrikes

let isStriking knight runState controllerState =
    knight.state = Striking && not <| canStrike runState controllerState

let processInAir velocity runState (worldState, controllerState) = 
    let knight = worldState.knight
    let walkCommand = getWalkCommand runState
    let direction = match walkCommand with Some dir -> dir | None -> knight.direction

    let nv = min (velocity + gravityStrength) terminalVelocity
    let (positionAfterVertical, verticalSpeed) = tryApplyVelocity nv knight.position worldState.blocks
    let finalPosition = 
        match walkCommand with 
        | Some dir -> tryWalk dir positionAfterVertical worldState.blocks
        | None -> positionAfterVertical

    let hasHitSpikes = tryFindCollision South finalPosition worldState.spikes
    let newKnight = 
        { knight with 
            position = finalPosition
            direction = direction
            verticalSpeed = verticalSpeed
            state = match hasHitSpikes with Some _ -> Dying | _ -> Walking
            timeOfDeath = match hasHitSpikes with Some _ -> Some runState.elapsed | _ -> None }

    { worldState with knight = newKnight }, controllerState

let processOnGround runState (worldState, controllerState) =
    let knight = worldState.knight
    
    if isStriking knight runState controllerState then
        (worldState, controllerState)
    else if strikeKeys |> runState.IsAnyPressed && canStrike runState controllerState then
        let newKnight = { knight with  state = Striking }
        { worldState with knight = newKnight }, { controllerState with lastStrikeTime = runState.elapsed }
    else 
        let walkCommand = getWalkCommand runState
        let direction = match walkCommand with Some dir -> dir | None -> knight.direction

        if blockKeys |> runState.IsAnyPressed then
            let newKnight = 
                { knight with 
                    direction = direction
                    state = Blocking }
            { worldState with knight = newKnight }, controllerState
        else if jumpKeys |> runState.WasAnyJustPressed then
            let newKnight = 
                { knight with 
                    direction = direction
                    verticalSpeed = Some jumpSpeed
                    state = Walking }
            { worldState with knight = newKnight }, controllerState
        else
            let (position, state) = 
                match walkCommand with
                | Some dir -> tryWalk dir knight.position worldState.blocks, Walking
                | None -> knight.position, Standing
            let newKnight = 
                { knight with 
                    position = position
                    direction = direction
                    state = state }
            { worldState with knight = newKnight }, controllerState

let processKnight runState (worldState, controllerState) =
    let knight = worldState.knight
    match knight.timeOfDeath with
    | Some t ->
        if runState.elapsed - t < timeForDying then
            { worldState with knight = { worldState.knight with state = Dying } }, controllerState
        else
            { worldState with knight = { worldState.knight with state = Dead } }, controllerState
    | None ->
        match knight.verticalSpeed with
        | Some velocity ->
            processInAir velocity runState (worldState, controllerState)
        | None ->
            let (_,gravityEffect) = tryApplyVelocity gravityStrength knight.position worldState.blocks
            match gravityEffect with
            | Some v ->
                let newKnight = { knight with verticalSpeed = Some v }
                { worldState with knight = newKnight }, controllerState
            | None ->
                processOnGround runState (worldState, controllerState)