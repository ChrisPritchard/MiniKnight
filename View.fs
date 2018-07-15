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

let statics (kx,ky) =
    List.map (fun (x,y,kind) ->
        let ox, oy = int (float x - kx) * blockWidth, int (float y - ky) * blockHeight
        let destRect = (centreX + ox, centreY + oy, blockWidth, blockHeight)
        match kind with
        | Block -> MappedImage ("stoneFloor", "stoneFloor_15", destRect)
        | Spikes -> Image ("spikes", destRect)
        | _ -> failwith "Not implemented yet")

let getKnightFrame (knight : Knight) elapsed = 
    let byDir leftFrame rightFrame = if knight.direction = Left then leftFrame else rightFrame
    let gameFrame = elapsed / animSpeed
    let frameFor max = gameFrame % float max |> int
    let numberedFrame leftStart rightStart maxFrame =
        let frame = (if knight.direction = Left then leftStart else rightStart) + frameFor maxFrame
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
        yield! statics state.knight.position state.level
        yield MappedImage ("knight", getKnightFrame state.knight elapsed, (centreX, centreY, blockWidth, blockHeight))
    } |> Seq.toList

let getView runState model =
    match model with
    | Playing state ->
        getPlayingView runState state
    | _ -> []