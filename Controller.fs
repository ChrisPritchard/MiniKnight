module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let mutable lastCommandTime = 0.

let checkForDirectionChange runState current =
    if runState.elapsed - lastCommandTime < timeBetweenCommands then current
    else if runState.WasJustPressed Keys.Left && current <> Left then
        lastCommandTime <- runState.elapsed
        Left
    else if runState.WasJustPressed Keys.Right && current <> Right then
        lastCommandTime <- runState.elapsed
        Right
    else
        current

let handlePlayingState runState state =
    let knightDir = checkForDirectionChange runState state.knight.direction

    let newState = { state with knight = { state.knight with direction = knightDir } }
    Some (Playing newState)

let advanceGame (runState : RunState) =
    function
    | None -> Some startWorld
    | Some world -> 
        match world with
        | Playing state -> handlePlayingState runState state 
        | _ -> Some world
