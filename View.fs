module View

open GameCore
open Model

let animSpeed = 200.

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

let screenWidth, screenHeight = 800, 600
let resolution = Windowed (screenWidth, screenHeight)
let blockWidth, blockHeight = 40, 40
let centreX, centreY = (screenWidth / 2) - (blockWidth / 2), (screenHeight / 2) - (blockHeight / 2)

let validAdjacents = 
    [
        "00111000";"00111110";"00001110";"00001000";"11111000";"11111111";"10001111";"10001000";
        "11100000";"11100011";"10000011";"10000000";"00100000";"00100010";"00000010";"00000000"
    ]
let adjacencyKey list = 
    let key = 
        [(0, -1);(1, -1);(1, 0);(1, 1);(0, 1);(-1, 1);(-1, 0);(-1, -1)]
        |> List.map (fun pos -> if List.contains pos list then "1" else "0")
        |> String.concat ""
    if List.contains key validAdjacents then key else "00000000"

let frameFor max elapsed = 
    let gameFrame = elapsed / animSpeed
    gameFrame % float max |> int

let blockFrame (x, y) level = 
    let adjacent = level |> List.filter (fun (ox,oy,kind) ->
        match kind with
        | Block when abs (ox - x) < 2 && abs (oy - y) < 2 -> true
        | _ -> false) |> List.map (fun (ox, oy, _) -> (ox - x, oy - y))
    sprintf "floor%s" <| adjacencyKey adjacent

let statics (knightX, knightY) elapsed level =
    let kwx, kwy = int (knightX * float blockWidth), int (knightY * float blockHeight)
    level |> List.map (fun (x,y,kind) ->
        let wx, wy = x * blockWidth - kwx, y * blockHeight - kwy
        let destRect = (centreX + wx, centreY + wy, blockWidth, blockHeight)
        match kind with
        | Block -> 
            let frame = blockFrame (x,y) level 
            MappedImage ("stoneFloor", frame, destRect) // todo frame from adjacents
        | Spikes -> Image ("spikes", destRect)
        | Coin -> 
            let frame = sprintf "goldCoinSm_%i" <| frameFor 10 elapsed
            MappedImage ("goldCoin", frame, destRect)
        | EntryPortal -> 
            let frame = sprintf "portal-arrive_%i" <| frameFor 2 elapsed
            MappedImage ("portalArrive", frame, destRect)
        | ExitPortal -> 
            let frame = sprintf "portal_%i" <| frameFor 2 elapsed
            MappedImage ("portalDepart", frame, destRect))

let getKnightFrame (knight : Knight) elapsed = 
    let byDir leftFrame rightFrame = if knight.direction = Left then leftFrame else rightFrame
    let numberedFrame leftStart rightStart maxFrame =
        let frame = (if knight.direction = Left then leftStart else rightStart) + (frameFor maxFrame elapsed)
        sprintf "MiniKnight_%i" frame

    match knight.state with
    | Standing | Jumping _ -> byDir "standleft1" "standright1"
    | Walking -> numberedFrame 6 15 4
    | Striking -> numberedFrame 24 26 2
    | Blocking -> byDir "guardleft1" "guardright1"
    | Hit _ -> numberedFrame 2 4 2
    | Dying -> numberedFrame 10 19 5
    | Dead -> byDir "deadleft2" "deadright2"

let getPlayingView (runState : RunState) (state : PlayingState) =
    let elapsed = runState.elapsed
    seq {
        yield Image ("background", (0,0,screenWidth,screenHeight))
        yield! statics state.knight.position elapsed state.level
        yield MappedImage ("knight", getKnightFrame state.knight elapsed, (centreX, centreY, blockWidth, blockHeight))
    } |> Seq.toList

let getView runState model =
    match model with
    | Playing state ->
        getPlayingView runState state
    | _ -> []