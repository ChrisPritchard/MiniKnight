module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let timeForAttacks = 200.
let timeBetweenMovement = 25.
let timeBetweenGravity = 25.
let walkSpeed = 0.15
let jumpSpeed = -0.55
let gravityStrength = 0.05
let terminalSpeed = 0.9

let collision (x, y) blocks =
    let hor = (floor x |> int), (floor y |> int)
    let ver = (ceil x |> int), (ceil y |> int)
    List.exists (fun (mx, my, _) -> (mx, my) = hor || (mx, my) = ver) blocks

let checkForFallingPosChange (runState: RunState) (worldState, controllerState) =
    if runState.elapsed - controllerState.lastGravityTime < timeBetweenGravity then
        worldState,
        controllerState
    else
        let knight = worldState.knight;
        let newFallSpeed = knight.fallSpeed + gravityStrength
        let newFallSpeed = if abs newFallSpeed > terminalSpeed then knight.fallSpeed else newFallSpeed
        let (x, y) = knight.position;
        let newY = y + newFallSpeed;
        let isCollision = collision (x, newY)  worldState.blocks
        let newPos = if isCollision then (x, y) else (x, newY)
        let newFallSpeed = if isCollision then 0. else newFallSpeed
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
    let canCommand = runState.elapsed - controllerState.lastCommandTime >= timeBetweenCommands

    if worldState.knight.state = Striking && runState.elapsed - controllerState.lastAttackTime < timeForAttacks 
    then
        worldState, 
        controllerState
    else if canCommand && (runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl) 
    then
        worldState.withKnightState Striking,
        { controllerState with
            lastCommandTime = runState.elapsed
            lastAttackTime = runState.elapsed }
    else if canCommand && worldState.knight.fallSpeed = 0. && (runState.WasJustPressed Keys.Space || runState.WasJustPressed Keys.W) 
    then
        worldState.withKnightFallSpeed jumpSpeed,
        { controllerState with
            lastCommandTime = runState.elapsed }
    else if anyPressed [Keys.LeftAlt;Keys.RightAlt] 
    then 
        worldState.withKnightState Blocking,
        controllerState
    else if anyPressed [Keys.Left;Keys.A;Keys.D;Keys.Right] 
    then 
        worldState.withKnightState Walking,
        controllerState
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