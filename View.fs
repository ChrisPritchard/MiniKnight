module View

open GameCore
open Model

let assetsToLoad = [
    Texture { key = "background"; path = "./Content/Sprites/background.png" }
    Texture { key = "knight"; path = "./Content/Sprites/knight.png" }
    Texture { key = "orc"; path = "./Content/Sprites/orc.png" }
    Texture { key = "stoneFloor"; path = "./Content/Sprites/stoneFloor.png" }
    Texture { key = "goldCoin"; path = "./Content/Sprites/goldCoin.png" }
    Texture { key = "spikes"; path = "./Content/Sprites/spikes.png" }
    Texture { key = "portalArrive"; path = "./Content/Sprites/portalArrive.png" }
    Texture { key = "portalDepart"; path = "./Content/Sprites/portalDepart.png" }
]

let screenWidth, screenHeight = 800, 600
let resolution = Windowed (screenWidth, screenHeight)

let getPlayingView runState gameState = 
    [
        Image { assetKey = "background"; destRect = 0,0,screenWidth,screenHeight; sourceRect = None}
    ]

let getView runState model =
    match model with
    | Playing state ->
        getPlayingView runState state
    | _ -> []