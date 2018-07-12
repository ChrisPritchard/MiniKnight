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

let getKnightFrame (knight : Knight) elapsed = 
    let byDir leftFrame rightFrame = if knight.direction = Left then leftFrame else rightFrame
    let gameFrame = elapsed / animSpeed
    let frameFor max = gameFrame % float max |> int

    match knight.state with
    | Standing | Jumping _ -> byDir "standleft1" "standright1"
    | Walking -> 
        let frame = (if knight.direction = Left then 6 else 15) + frameFor 4
        sprintf "MiniKnight_%i" frame
    | Striking ->
        let frame = (if knight.direction = Left then 6 else 15) + frameFor 2
        sprintf "MiniKnight_%i" frame
    | Blocking -> byDir "guardleft1" "guardright1"
    | Dead -> byDir "deadleft2" "deadright2"
    | _ -> "standright1"

let getPlayingView (runState : RunState) (state : PlayingState) =
    let elapsed = runState.elapsed
    [
        Image ("background", (0,0,screenWidth,screenHeight), None)
        MappedImage ("knight", getKnightFrame state.knight elapsed, (screenWidth/2, screenHeight/2, 40, 40))
    ]

let getView runState model =
    match model with
    | Playing state ->
        getPlayingView runState state
    | _ -> []