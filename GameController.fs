module GameController

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input

let handlePlayingState runState worldState =
    worldState
    |> KnightController.processKnight runState
    |> Playing |> Some

let firstLevel = MapLoader.getLevel 1

let advanceGame (runState : RunState) =
    function
    | None -> 
        getLevelModel firstLevel runState.elapsed |> Some 
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing worldState when 
                worldState.knight.state = Dead && 
                runState.WasJustPressed Keys.R -> 
            getLevelModel firstLevel runState.elapsed |> Some
        | Playing worldState when 
            (match worldState.knight.state with
            | WarpingOut t when runState.elapsed - t > warpTime -> true 
            | _ -> false) -> 
            getLevelModel firstLevel runState.elapsed |> Some
        | Playing worldState -> 
            handlePlayingState runState worldState
        | _ -> Some model