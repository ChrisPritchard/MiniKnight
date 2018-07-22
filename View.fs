module View

open GameCore
open Model

let animSpeed = 200.
let screenWidth, screenHeight = 800, 600
let blockWidth, blockHeight = 40, 40

let assetsToLoad = [
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
        MappedImage ("stoneFloor", sprintf "floor%s" adjacency, destRect))

let spikes knightPos = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectFor (x, y) knightBlock
        Image ("spikes", destRect))

let coins knightPos elapsed = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectFor (x, y) knightBlock
        let frame = sprintf "goldCoinSm_%i" <| frameFor 10 elapsed
        MappedImage ("goldCoin", frame, destRect))

let portal knightPos elapsed isEntry (x, y) = 
    let destRect = relRectFor (x, y) <| blockFor knightPos
    if isEntry then
        let frame = sprintf "portal-arrive_%i" <| frameFor 2 elapsed
        MappedImage ("portalArrive", frame, destRect)
    else
        let frame = sprintf "portal_%i" <| frameFor 2 elapsed
        MappedImage ("portalDepart", frame, destRect)

let getKnightFrame (knight : Knight) elapsed lastStrikeTime = 
    let byDir leftFrame rightFrame = if knight.direction = Left then leftFrame else rightFrame
    let numberedFrame leftStart rightStart maxFrame =
        let frame = (if knight.direction = Left then leftStart else rightStart) + (frameFor maxFrame elapsed)
        sprintf "MiniKnight_%i" frame
    let strikeFrame = 
        let index = if elapsed - lastStrikeTime < animSpeed then 1 else 0
        byDir (sprintf "MiniKnight_%i" (24 + index)) (sprintf "MiniKnight_%i" (26 + index))
    match knight.state with
    | Standing -> byDir "standleft1" "standright1"
    | Walking -> numberedFrame 6 15 4
    | Striking -> strikeFrame
    | Blocking -> byDir "guardleft1" "guardright1"
    | Hit _ -> numberedFrame 2 4 2
    | Dying -> numberedFrame 10 19 5
    | Dead -> byDir "deadleft2" "deadright2"

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

let getPlayingView runState worldState lastStrikeTime =
    let elapsed = runState.elapsed
    let knightPos = worldState.knight.position
    seq {
        yield Image ("background", (0,0,screenWidth,screenHeight))
        yield! blocks knightPos worldState.blocks
        yield! spikes knightPos worldState.spikes
        yield! coins knightPos elapsed worldState.coins
        yield portal knightPos elapsed true worldState.entryPortal
        yield portal knightPos elapsed false worldState.exitPortal
         
        let frame = getKnightFrame worldState.knight elapsed lastStrikeTime
        let rect = getKnightRect frame
        yield MappedImage ("knight", frame, rect)
    } |> Seq.toList

let getView runState model =
    match model with
    | Playing (worldState, controllerState) ->
        getPlayingView runState worldState controllerState.lastStrikeTime
    | _ -> []