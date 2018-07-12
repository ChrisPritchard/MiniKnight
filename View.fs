module View

open GameCore
open Model

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

let getPlayingView _ _ = 
    [
        Image ("background", (0,0,screenWidth,screenHeight), None)
        MappedImage ("knight", "standright2", (screenWidth/2, screenHeight/2, 40, 40))
    ]

let getView runState model =
    match model with
    | Playing state ->
        getPlayingView runState state
    | _ -> []