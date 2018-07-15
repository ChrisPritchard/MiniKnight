module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let mutable lastCommandTime = 0.

let timeForAttacks = 200.
let mutable lastAttackTime = 0.

let timeBetweenPhysics = 25.
let mutable lastPhysicsTime = 0.

let walkSpeed = 0.1

let checkForDirectionChange (runState : RunState) current =
    let justPressed = List.exists runState.WasJustPressed
    if runState.elapsed - lastCommandTime < timeBetweenCommands then 
        current
    else if justPressed [Keys.Left; Keys.A] && current <> Left then
        lastCommandTime <- runState.elapsed
        Left
    else if justPressed [Keys.Right; Keys.D] && current <> Right then
        lastCommandTime <- runState.elapsed
        Right
    else
        current

let checkForStateChange (runState : RunState) knight =
    let anyPressed = List.exists (fun k -> List.contains k runState.keyboard.pressed)
    if knight.state = Striking && runState.elapsed - lastAttackTime < timeForAttacks then
        Striking
    else if anyPressed [Keys.LeftAlt;Keys.RightAlt] then 
        Blocking
    else if anyPressed [Keys.Left;Keys.A;Keys.D;Keys.Right] then 
        Walking
    else if runState.elapsed - lastCommandTime < timeBetweenCommands then 
        Standing
    else
        if runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl then
            lastCommandTime <- runState.elapsed
            lastAttackTime <- runState.elapsed
            Striking
        else 
            Standing

let collision (x, y) level =
    let tx, ty = (floor x |> int), (floor y |> int)
    List.exists (fun (mx, my, kind) -> 
        match kind with 
        | Block when (mx = tx || mx = tx + 1) && my = ty -> true 
        | _ -> false) level

let checkForPosChange runState level (knight : Knight) =
    if runState.elapsed - lastPhysicsTime < timeBetweenPhysics then
        knight.position
    else
        let (x, y) = knight.position
        match knight.state with
        | Walking -> 
            lastPhysicsTime <- runState.elapsed
            let newX = if knight.direction = Left then x - walkSpeed else x + walkSpeed
            if collision (newX, y) level then (x, y) else (newX, y)
        | Jumping _ -> (x, y)
        | _ -> (x, y)

let handlePlayingState runState worldState controllerState =
    let knightDir = checkForDirectionChange runState worldState.knight.direction
    let knightState = checkForStateChange runState worldState.knight
    let newKnight = { worldState.knight with direction = knightDir; state = knightState }
    
    let knightPos = checkForPosChange runState worldState.level newKnight
    let newState = { worldState with knight = { newKnight with position = knightPos } }

    Some (Playing (newState, controllerState))

let advanceGame (runState : RunState) =
    function
    | None -> Some startModel
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing (worldState, controllerState) -> handlePlayingState runState worldState controllerState
        | _ -> Some model