module View

open GameCore
open Model
open Microsoft.Xna.Framework

let animSpeed = 100.
let screenWidth, screenHeight = 800, 600
let blockWidth, blockHeight = 40, 40

let strikeFrames = 2
let dyingFrames = 5
let warpTime = 1500.

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
    Sound ("coin1", "./Content/Sounds/Coins/coin.wav")
    Sound ("coin2", "./Content/Sounds/Coins/coin2.wav")
    Sound ("coin3", "./Content/Sounds/Coins/coin3.wav")
    Sound ("block1", "./Content/Sounds/Shared/block1.wav")
    Sound ("block2", "./Content/Sounds/Shared/block2.wav")
    Sound ("block3", "./Content/Sounds/Shared/block3.wav")
    Sound ("swing1", "./Content/Sounds/Shared/swing.wav")
    Sound ("swing2", "./Content/Sounds/Shared/swing2.wav")
    Sound ("swing3", "./Content/Sounds/Shared/swing3.wav")
    Sound ("orcHit1", "./Content/Sounds/Orcs/hit1.wav")
    Sound ("orcHit2", "./Content/Sounds/Orcs/hit2.wav")
    Sound ("orcDie", "./Content/Sounds/Orcs/die.wav")
    Sound ("knightHit1", "./Content/Sounds/Knight/hit1.wav")
    Sound ("knightHit2", "./Content/Sounds/Knight/hit2.wav")
    Sound ("knightHit3", "./Content/Sounds/Knight/hit3.wav")
    Sound ("knightDie", "./Content/Sounds/Knight/die.wav")
    Sound ("jump", "./Content/Sounds/Knight/jump.wav")
    Sound ("walk1", "./Content/Sounds/Knight/walk1.wav")
    Sound ("walk2", "./Content/Sounds/Knight/walk2.wav")
    Sound ("hitSpikes", "./Content/Sounds/Spikes.wav")
    Sound ("warping", "./Content/Sounds/Portal.wav")
]

let resolution = Windowed (screenWidth, screenHeight)
let centreX, centreY = (screenWidth / 2) - (blockWidth / 2), (screenHeight / 2) - (blockHeight / 2)

let frameFor max elapsed = 
    let gameFrame = elapsed / animSpeed
    gameFrame % float max |> int

let blockFor (x, y) =
    int (x * float blockWidth), int (y * float blockHeight)

let relRectForStatic (x, y) (relX, relY) =
    let relX, relY = x * blockWidth - relX, y * blockHeight - relY
    centreX + relX, centreY + relY, blockWidth, blockHeight


let getOrcRect frame (x,y) = 
    let strikeWidth = float blockWidth * 1.5 |> int
    let blockingWidth = float blockWidth * 0.8 |> int
    match frame with
    | "Orc_25" ->
        (x - (strikeWidth - blockWidth), y, strikeWidth, blockHeight)
    | "Orc_27" ->
        (x, y, strikeWidth, blockHeight)
    | "guardLeft1" ->
        (x, y, blockingWidth, blockHeight)
    | "guardRight1" ->
        (x + (blockWidth - blockingWidth), y, blockingWidth, blockHeight)
    | _ -> 
        (x, y, blockWidth, blockHeight)

let relRectForOrc (x, y) (kx, ky) frame =
    let bw, bh = float blockWidth, float blockHeight
    let relX, relY = int <| x * bw - (kx * bw), int <| y * bh - (ky * bh)
    getOrcRect frame (centreX + relX, centreY + relY)

let blocks knightPos = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y, adjacency) ->
        let destRect = relRectForStatic (x, y) knightBlock
        MappedImage ("stoneFloor", sprintf "floor%s" adjacency, destRect, Color.White))

let spikes knightPos = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectForStatic (x, y) knightBlock
        Image ("spikes", destRect, Color.White))

let coins knightPos elapsed = 
    let knightBlock = blockFor knightPos
    List.map (fun (x, y) ->
        let destRect = relRectForStatic (x, y) knightBlock
        let frame = sprintf "goldCoinSm_%i" <| frameFor 10 elapsed
        MappedImage ("goldCoin", frame, destRect, Color.White))

let portal knightPos elapsed isEntry (x, y) = 
    let destRect = relRectForStatic (x, y) <| blockFor knightPos
    if isEntry then
        let frame = sprintf "portal-arrive_%i" <| frameFor 2 elapsed
        MappedImage ("portalArrive", frame, destRect, Color.White)
    else
        let frame = sprintf "portal_%i" <| frameFor 2 elapsed
        MappedImage ("portalDepart", frame, destRect, Color.White)

let getOrcFrame (orc : Orc) elapsed = 
    let byDir leftFrame rightFrame = if orc.direction = Left then leftFrame else rightFrame
    let numberedFrame leftStart rightStart maxFrame elapsed =
        let frame = (if orc.direction = Left then leftStart else rightStart) + (frameFor maxFrame elapsed)
        sprintf "Orc_%i" frame
    let strikeFrame startTime = 
        let index = if elapsed - startTime < animSpeed then 1 else 0
        byDir (sprintf "Orc_%i" (24 + index)) (sprintf "Orc_%i" (26 + index))
    match orc.state with
    | Patrolling -> numberedFrame 6 15 4 elapsed
    | Attacking t -> strikeFrame t
    | Guarding _ -> byDir "guardLeft1" "guardRight1"
    | Falling t -> numberedFrame 10 19 dyingFrames (elapsed - t)
    | Slain -> byDir "deadLeft2" "deadRight2"
    | _ -> byDir "standLeft2" "standRight2"

let orcs knightPos elapsed =
    List.map (fun (orc: Orc) -> 
        let frame = getOrcFrame orc elapsed
        let destRect = relRectForOrc orc.position knightPos frame
        MappedImage ("orc", frame, destRect, Color.White))

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
        let a = (elapsed - startTime) / warpTime
        new Color (Color.White, float32 a)
    | WarpingOut startTime -> 
        let a = (elapsed - startTime) / warpTime
        new Color (Color.White, float32 <| 1. - a)
    | _ -> Color.White

let sounds elapsed = 
    let indexed key max = sprintf "%s%i" key <| int elapsed % max + 1
    List.map (function            
    | OrcSwing | KnightSwing -> 
        SoundEffect <| indexed "swing" 3
    | OrcBlocked | KnightBlocked -> 
        SoundEffect <| indexed "block" 3
    | OrcHit -> 
        SoundEffect <| indexed "orcHit" 2
    | OrcFalling -> 
        SoundEffect "orcDie"
    | Jump -> 
        SoundEffect "jump"
    | KnightHit -> 
        SoundEffect <| indexed "knightHit" 3
    | KnightDying -> 
        SoundEffect "knightDie"
    | CoinCollect ->
        SoundEffect <| indexed "coin" 3
    | Warping -> 
        SoundEffect "warping"
    | HitSpikes -> 
        SoundEffect "hitSpikes")

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

        yield! orcs knightPos elapsed worldState.orcs
         
        let frame = getKnightFrame knight elapsed
        let rect = getKnightRect frame
        let colour = getKnightColour knight elapsed
        yield MappedImage ("knight", frame, rect, colour)

        match knight.state with
        | Dead ->
            yield Text ("default", "You Died!", (screenWidth / 2, screenHeight / 2), Centre, 1.5, Color.White)
            yield Text ("default", sprintf "Score before death: %i pts" knight.score, (screenWidth / 2, screenHeight / 2 + 50), Centre, 0.8, Color.White)
            yield Text ("default", "Press 'R' to try again", (screenWidth / 2, screenHeight / 2 + 90), Centre, 0.8, Color.White)
        | _ -> 
            yield Text ("default", sprintf "score: %i pts" knight.score, (20, screenHeight - 30), TopLeft, 0.5, Color.White)

        yield! sounds elapsed worldState.events
        if knight.state = Walking && knight.verticalSpeed = None then
            if int elapsed % 600 = 0 then
                yield SoundEffect "walk1"
            else if int elapsed % 300 = 0 then
                yield SoundEffect "walk2"
    } |> Seq.toList

let getView runState model =
    match model with
    | Playing worldState ->
        getPlayingView runState worldState
    | _ -> []