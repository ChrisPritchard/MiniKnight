module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input
open Aether.Operators

let timeBetweenCommands = 100.
let timeForAttacks = 200.
let timeBetweenPhysics = 25.
let walkSpeed = 0.1

let checkForDirectionChange (runState: RunState) worldState (controllerState: ControllerState) =
    let justPressed = List.exists runState.WasJustPressed
    let current = worldState.knight.direction

    if runState.elapsed - controllerState.lastCommandTime < timeBetweenCommands 
    then 
        worldState, 
        controllerState
    else if justPressed [Keys.Left; Keys.A] && current <> Left 
    then
        worldState |> Left ^= knightDirection,
        { controllerState with lastCommandTime = runState.elapsed }
    else if justPressed [Keys.Right; Keys.D] && current <> Right 
    then
        worldState |> Right ^= knightDirection,
        { controllerState with lastCommandTime = runState.elapsed }
    else
        worldState, 
        controllerState

let checkForStateChange runState worldState controllerState =
    let anyPressed = List.exists (fun k -> List.contains k runState.keyboard.pressed)
    if worldState.knight.state = Striking && runState.elapsed - controllerState.lastAttackTime < timeForAttacks 
    then
        worldState, 
        controllerState
    else if anyPressed [Keys.LeftAlt;Keys.RightAlt] 
    then 
        worldState |> Blocking ^= knightState,
        controllerState
    else if anyPressed [Keys.Left;Keys.A;Keys.D;Keys.Right] then 
        worldState |> Walking ^= knightState,
        controllerState
    else if runState.elapsed - controllerState.lastCommandTime < timeBetweenCommands then 
        worldState |> Standing ^= knightState,
        controllerState
    else
        if runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl then
            worldState |> Striking ^= knightState,
            { controllerState with
                lastCommandTime = runState.elapsed
                lastAttackTime = runState.elapsed }
        else 
            worldState |> Standing ^= knightState,
            controllerState

let collision (x, y) level =
    let tx, ty = (floor x |> int), (floor y |> int)
    List.exists (fun (mx, my, kind) -> 
        match kind with 
        | Block when (mx = tx || mx = tx + 1) && my = ty -> true 
        | _ -> false) level

let checkForPosChange runState worldState controllerState =
    let knight = worldState.knight
    if runState.elapsed - controllerState.lastPhysicsTime < timeBetweenPhysics then
        worldState,
        controllerState
    else
        let (x, y) = knight.position
        match knight.state with
        | Walking -> 
            let newX = if knight.direction = Left then x - walkSpeed else x + walkSpeed
            let newPos = if collision (newX, y) worldState.level then (x, y) else (newX, y)
            worldState |> newPos ^= knightPosition,
            { controllerState with lastPhysicsTime = runState.elapsed }
        | Jumping _ -> 
            worldState, // todo
            controllerState
        | _ -> 
            worldState,
            controllerState

let handlePlayingState runState worldState controllerState =
    let (worldState,controllerState) = checkForDirectionChange runState worldState controllerState
    let (worldState,controllerState) = checkForStateChange runState worldState controllerState
    let (worldState,controllerState) = checkForPosChange runState worldState controllerState

    Some (Playing (worldState,controllerState))

let advanceGame (runState : RunState) =
    function
    | None -> Some startModel
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing (worldState, controllerState) -> handlePlayingState runState worldState controllerState
        | _ -> Some model