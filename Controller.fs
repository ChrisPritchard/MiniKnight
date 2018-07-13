module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let mutable lastCommandTime = 0.

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
    if anyPressed [Keys.LeftAlt;Keys.RightAlt] then 
        Blocking
    // test
    else if anyPressed [Keys.X] then 
        Dying
    else if runState.elapsed - lastCommandTime < timeBetweenCommands then 
        Walking
    else
        if runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl then
            lastCommandTime <- runState.elapsed
            Striking
        else 
            Walking

let handlePlayingState runState state =
    let knightDir = checkForDirectionChange runState state.knight.direction
    let knightState = checkForStateChange runState state.knight

    let newState = { state with knight = { state.knight with direction = knightDir; state = knightState } }
    Some (Playing newState)

let advanceGame (runState : RunState) =
    function
    | None -> Some startWorld
    | Some world -> 
        match world with
        | Playing state -> handlePlayingState runState state 
        | _ -> Some world