module View

open GameCore
open Model
open Microsoft.Xna.Framework

let animSpeed = 100.
let screenWidth, screenHeight = 800, 600
let blockWidth, blockHeight = 40, 40

let strikeFrames = 2
let dyingFrames = 5
let warpTime = 3000.

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("background", "./Content/Sprites/background.png")
    TextureMap ("knight", "./Content/Sprites/knight.png", "./Content/Sprites/knight-key.csv")
    TextureMap ("orc", "./Content/Sprites/orc.png", "./Content/Sprites/orc-key.csv")
    TextureMap ("stoneFloor", "./Content/Sprites/stoneFloor.png", "./Content/Sprites/stoneFloor-key.csv")
    TextureMap ("goldCoin", "./Content/Sprites/goldCoin.png", "./Content/Sprites/goldCoin-key.csv")
    Texture ("spikes", "./Content/Sprites/spikes.png")
    TextureMap ("portalArrive", "./Content/Sprites/portalArrive.png", "./Content/Sprites/portalArrive-key.csv")
    TextureMap ("portalDepart", "./Content/Sprites/portalDepart.png", "./Content/Sprites/portalDepart-key.csv")
]

let resolution = Windowed (screenWidth, screenHeight)
let centreX, centreY = (screenWidth / 2) - (blockWidth / 2), (screenHeight / 2) - (blockHeight / 2)

let frameFor max elapsed = 
    let gameFrame = elapsed / animSpeed
    gameFrame % float max |> int

let blockFor (x, y) =
    int (x * float blockWidth), int (y * float blockHeight)

let relRectFor (x, y) (relX, relY) =
    let relX, relY = x * blockWidth - relX, y * blockHeight - relY
    centreX + relX, centreY + relY, blockWidth, blockHeight

let blocks knightPos = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y, adjacency) ->
        let destRect = relRectFor (x, y) knightBlock
        MappedImage ("stoneFloor", sprintf "floor%s" adjacency, destRect, Color.White))

let spikes knightPos = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectFor (x, y) knightBlock
        Image ("spikes", destRect, Color.White))

let coins knightPos elapsed = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectFor (x, y) knightBlock
        let frame = sprintf "goldCoinSm_%i" <| frameFor 10 elapsed
        MappedImage ("goldCoin", frame, destRect, Color.White))

let portal knightPos elapsed isEntry (x, y) = 
    let destRect = relRectFor (x, y) <| blockFor knightPos
    if isEntry then
        let frame = sprintf "portal-arrive_%i" <| frameFor 2 elapsed
        MappedImage ("portalArrive", frame, destRect, Color.White)
    else
        let frame = sprintf "portal_%i" <| frameFor 2 elapsed
        MappedImage ("portalDepart", frame, destRect, Color.White)

let getKnightFrame (knight : Knight) elapsed = 
    let byDir leftFrame rightFrame = if knight.direction = Left then leftFrame else rightFrame
    let numberedFrame leftStart rightStart maxFrame elapsed =
        let frame = (if knight.direction = Left then leftStart else rightStart) + (frameFor maxFrame elapsed)
        sprintf "MiniKnight_%i" frame
    let strikeFrame startTime = 
        let index = if elapsed - startTime < animSpeed then 1 else 0
        byDir (sprintf "MiniKnight_%i" (24 + index)) (sprintf "MiniKnight_%i" (26 + index))
    match knight.state with
    | Walking -> numberedFrame 6 15 4 elapsed
    | Striking t -> strikeFrame t
    | Blocking -> byDir "guardleft1" "guardright1"
    | Hit t -> numberedFrame 2 4 2 (elapsed - t)
    | Dying t -> numberedFrame 10 19 dyingFrames (elapsed - t)
    | Dead -> byDir "deadLeft2" "deadRight2"
    | _ -> byDir "standleft1" "standright1"

let getKnightRect frame = 
    let strikeWidth = float blockWidth * 1.5 |> int
    let blockingWidth = float blockWidth * 0.8 |> int
    match frame with
    | "MiniKnight_25" ->
        (centreX - (strikeWidth - blockWidth), centreY, strikeWidth, blockHeight)
    | "MiniKnight_27" ->
        (centreX, centreY, strikeWidth, blockHeight)
    | "guardleft1" ->
        (centreX, centreY, blockingWidth, blockHeight)
    | "guardright1" ->
        (centreX + (blockWidth - blockingWidth), centreY, blockingWidth, blockHeight)
    | _ -> 
        (centreX, centreY, blockWidth, blockHeight)

let getKnightColour knight elapsed = 
    match knight.state with
    | WarpingIn startTime -> 
        let a = ((warpTime / (elapsed - startTime)) * 256.) |> int
        new Color (Color.White, a)
    | WarpingOut startTime -> 
        let a = ((warpTime / (elapsed - startTime)) * 256.) |> int
        new Color (Color.White, 256 - a)
    | _ -> Color.White

let getPlayingView runState worldState =
    let elapsed = runState.elapsed
    let knight = worldState.knight
    let knightPos = knight.position
    seq {
        yield Image ("background", (0,0,screenWidth,screenHeight), Color.White)
        yield! blocks knightPos worldState.blocks
        yield! spikes knightPos worldState.spikes
        yield! coins knightPos elapsed worldState.coins
        yield portal knightPos elapsed true worldState.entryPortal
        yield portal knightPos elapsed false worldState.exitPortal
         
        let frame = getKnightFrame knight elapsed
        let rect = getKnightRect frame
        let colour = getKnightColour knight elapsed
        yield MappedImage ("knight", frame, rect, colour)

        match knight.state with
        | Dead ->
            yield Text ("default", "Game Over", (screenWidth / 2, screenHeight / 2), Centre, 1.5, Color.White)
            yield Text ("default", sprintf "Score for this level: %i pts" knight.score, (screenWidth / 2, screenHeight / 2 + 50), Centre, 0.8, Color.White)
            yield Text ("default", "Press 'R' to try again", (screenWidth / 2, screenHeight / 2 + 90), Centre, 0.8, Color.White)
        | _ -> 
            yield Text ("default", sprintf "score: %i pts" knight.score, (20, screenHeight - 30), TopLeft, 0.5, Color.White)
    } |> Seq.toList

let getView runState model =
    match model with
    | Playing worldState ->
        getPlayingView runState worldState
    | _ -> []