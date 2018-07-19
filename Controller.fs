module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let timeForAttacks = 200.
let timeBetweenMovement = 25.
let timeBetweenGravity = 25.
let walkSpeed = 0.1
let jumpSpeed = -1.
let gravityStrength = 0.1

let collision (x, y) blocks =
    let tx, ty = (floor x |> int), (floor y |> int)
    List.exists (fun (mx, my, _) -> (mx = tx || mx = tx + 1) && my = ty) blocks

let checkForFallingPosChange (runState: RunState) (worldState, controllerState) =
    if runState.elapsed - controllerState.lastGravityTime < timeBetweenGravity then
        worldState,
        controllerState
    else
        let knight = worldState.knight;
        let newFallSpeed = knight.fallSpeed + gravityStrength
        let (x, y) = knight.position;
        let newY = y + newFallSpeed;
        let newPos = if collision (x, newY) worldState.blocks then (x, y) else (x, newY)
        (worldState.withKnightPosition newPos).withKnightFallSpeed newFallSpeed,
        { controllerState with lastGravityTime = runState.elapsed }

let checkForDirectionChange (runState: RunState) (worldState, controllerState) =
    let justPressed = List.exists runState.WasJustPressed
    let current = worldState.knight.direction

    if runState.elapsed - controllerState.lastCommandTime < timeBetweenCommands 
    then 
        worldState, 
        controllerState
    else if justPressed [Keys.Left; Keys.A] && current <> Left 
    then
        worldState.withKnightDirection Left,
        { controllerState with lastCommandTime = runState.elapsed }
    else if justPressed [Keys.Right; Keys.D] && current <> Right 
    then
        worldState.withKnightDirection Right,
        { controllerState with lastCommandTime = runState.elapsed }
    else
        worldState, 
        controllerState

let checkForStateChange runState (worldState, controllerState) =
    let anyPressed = List.exists (fun k -> List.contains k runState.keyboard.pressed)
    if worldState.knight.state = Striking && runState.elapsed - controllerState.lastAttackTime < timeForAttacks 
    then
        worldState, 
        controllerState
    else if anyPressed [Keys.LeftAlt;Keys.RightAlt] 
    then 
        worldState.withKnightState Blocking,
        controllerState
    else if anyPressed [Keys.Left;Keys.A;Keys.D;Keys.Right] then 
        worldState.withKnightState Walking,
        controllerState
    else if runState.elapsed - controllerState.lastCommandTime < timeBetweenCommands then 
        worldState.withKnightState Standing,
        controllerState
    else
        if runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl then
            worldState.withKnightState Striking,
            { controllerState with
                lastCommandTime = runState.elapsed
                lastAttackTime = runState.elapsed }
        else if worldState.knight.fallSpeed = 0. && (runState.WasJustPressed Keys.Space || runState.WasJustPressed Keys.W) then
            worldState.withKnightFallSpeed jumpSpeed,
            { controllerState with
                lastCommandTime = runState.elapsed }
        else
            worldState.withKnightState Standing,
            controllerState

let checkForWalkingPosChange runState (worldState, controllerState) =
    let knight = worldState.knight
    if runState.elapsed - controllerState.lastMovementTime < timeBetweenMovement then
        worldState,
        controllerState
    else
        let (x, y) = knight.position
        match knight.state with
        | Walking -> 
            let newX = if knight.direction = Left then x - walkSpeed else x + walkSpeed
            let newPos = if collision (newX, y) worldState.blocks then (x, y) else (newX, y)
            worldState.withKnightPosition newPos,
            { controllerState with lastMovementTime = runState.elapsed }
        | _ -> 
            worldState,
            controllerState

let handlePlayingState runState worldState controllerState =
    (worldState, controllerState)
    |> checkForFallingPosChange runState
    |> checkForDirectionChange runState
    |> checkForStateChange runState
    |> checkForWalkingPosChange runState
    |> Playing |> Some

let advanceGame (runState : RunState) =
    function
    | None -> MapLoader.getLevel 1 |> getLevelModel |> Some 
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing (worldState, controllerState) -> 
            handlePlayingState runState worldState controllerState
        | _ -> Some model