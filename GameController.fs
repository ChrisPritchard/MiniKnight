module GameController

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input

let handlePlayingState runState worldState =
    worldState
    |> KnightController.processKnight runState
    |> OrcController.processOrcs runState
    |> Playing |> Some

let maxLevel = 5
let levels = [1..maxLevel] |> List.map (fun i -> (i, MapLoader.getLevel i)) |> Map.ofList

let hasReset (runState : RunState) worldState = 
    worldState.knight.state = Dead && runState.WasJustPressed Keys.R 

let hasWarpedOut (runState : RunState) worldState = 
    match worldState.knight.state with
    | WarpingOut t when runState.elapsed - t > warpTime -> true 
    | _ -> false

let advanceGame runState =
    function
    | None -> 
        getLevelModel levels.[1] 1 0 runState.elapsed |> Some 
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing worldState when hasReset runState worldState -> 
            Some <| getLevelModel 
                levels.[worldState.level] 
                worldState.level 
                worldState.knight.startScore 
                runState.elapsed
        | Playing worldState when hasWarpedOut runState worldState -> 
            if worldState.level = maxLevel then
                Some <| GameOver worldState.knight.score
            else
                Some <| getLevelModel 
                    levels.[worldState.level + 1] 
                    (worldState.level + 1)
                    worldState.knight.score 
                    runState.elapsed
        | Playing worldState -> 
            { worldState with events = [] } |> handlePlayingState runState
        | _ -> Some model