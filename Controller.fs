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

let walkSpeed = 0.05

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
            let newX = if knight.direction = Left then x - walkSpeed else x + walkSpeed
            if collision (newX, y) level then (x, y) else (newX, y)
        | Jumping _ -> (x, y)
        | _ -> (x, y)

let handlePlayingState runState state =
    let knightDir = checkForDirectionChange runState state.knight.direction
    let knightState = checkForStateChange runState state.knight
    let newKnight = { state.knight with direction = knightDir; state = knightState }
    
    let knightPos = checkForPosChange runState state.level newKnight
    let newState = { state with knight = { newKnight with position = knightPos } }

    Some (Playing newState)

let advanceGame (runState : RunState) =
    function
    | None -> Some startWorld
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some world -> 
        match world with
        | Playing state -> handlePlayingState runState state 
        | _ -> Some world