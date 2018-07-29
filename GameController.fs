module GameController

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input
open System.Drawing

let bubbleSpeed = 0.02
let bubbleHeight = 1.

let mapKey = [
    (Color.FromArgb(128, 128, 128), Block)
    (Color.FromArgb(255, 0, 0), Spikes)
    (Color.FromArgb(38, 127, 0), EntryPortal)
    (Color.FromArgb(0, 38, 255), ExitPortal)
    (Color.FromArgb(255, 216, 0), Coin)
    (Color.FromArgb(0, 0, 0), Orc)
]

let getLevel num = 
    use bitmap = Bitmap.FromFile <| sprintf "./Content/Maps/map%i.bmp" num :?> Bitmap
    [0..bitmap.Height-1] |> Seq.collect (fun y -> [0..bitmap.Width-1] |> Seq.map (fun x ->
        let color = bitmap.GetPixel (x, y)
        match Seq.tryFind (fun (c,_) -> c = color) mapKey with
        | Some t -> Some (x, y, t |> snd)
        | _ -> None)) |> Seq.choose id |> Seq.toList

let maxLevel = 5
let levels = [1..maxLevel] |> List.map (fun i -> (i, getLevel i)) |> Map.ofList

let hasReset (runState : RunState) worldState = 
    worldState.knight.state = Dead && runState.WasJustPressed Keys.R 

let hasWarpedOut (runState : RunState) worldState = 
    match worldState.knight.state with
    | WarpingOut t when runState.elapsed - t > warpTime -> true 
    | _ -> false

let processBubbles worldState = 
    { worldState with 
        bubbles =
            worldState.bubbles 
                |> List.map (fun b -> 
                    let (x, y) = b.position
                    { b with position = x, y - bubbleSpeed })
                |> List.filter (fun b -> 
                    let (_, y) = b.position
                    (b.startY - y) < bubbleHeight) }

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
        | Playing worldState when hasWarpedOut runState worldState && worldState.level = maxLevel -> 
            Some <| GameOver worldState.knight.score
        | Playing worldState when hasWarpedOut runState worldState ->
            Some <| getLevelModel 
                levels.[worldState.level + 1] 
                (worldState.level + 1)
                worldState.knight.score 
                runState.elapsed
        | Playing worldState -> 
            { worldState with events = [] }
            |> KnightController.processKnight runState
            |> OrcController.processOrcs runState
            |> processBubbles
            |> Playing |> Some            
        | _ -> Some model