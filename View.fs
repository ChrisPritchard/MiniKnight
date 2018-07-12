module View

open GameCore
open Model

let assetsToLoad = [
    Texture { key = "background"; path = "./Content/Sprites/background.png" }
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