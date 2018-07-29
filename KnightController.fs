module KnightController

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input

let walkSpeed = 0.15
let jumpSpeed = -0.6
let gravityStrength = 0.05
let terminalVelocity = 0.9

let coinScore = 2
let killScore = 10

let walkLeftKeys = [Keys.A;Keys.Left]
let walkRightKeys = [Keys.D;Keys.Right]
let jumpKeys = [Keys.W;Keys.Space]
let strikeKeys = [Keys.LeftControl;Keys.RightControl]
let blockKeys = [Keys.LeftAlt;Keys.RightAlt]

let private checkForVerticalBlocker (x, ny) isFalling =
    let (floory, ceily) = floor ny, ceil ny
    let isInVertical bx =
        bx = x ||
        (bx < x && (bx + 1.) > x) ||
        (bx > x && (bx - 1.) < x)

    List.tryFind (fun (bx, by) ->
        isInVertical bx && 
            match isFalling with
            | false -> by = ceily - 1.
            | true -> by = floory + 1.)

let tryApplyVelocity verticalSpeed (x, y) worldState =
    let ny = y + verticalSpeed
    let blockers = 
        [
            worldState.blocks |> List.map (fun (bx, by, _) -> (float bx, float by));
            worldState.orcs |> List.filter (fun o -> o.state <> Slain) |> List.map (fun o -> o.position)
        ] |> List.concat
    if verticalSpeed < 0. then
        let ceiling = checkForVerticalBlocker (x, ny) false blockers
        match ceiling with
        | Some (_, by) -> (x, float by + 1.), Some 0.
        | None -> (x, ny), Some verticalSpeed
    else
        let floor = checkForVerticalBlocker (x, ny) true blockers
        match floor with
        | Some (_, by) -> (x, float by - 1.), None
        | None -> (x, ny), Some verticalSpeed

let checkForHorizontalBlocker (nx, y) direction = 
    let (floorx, ceilx) = floor nx, ceil nx
    let isInHorizontal by =
        by = y ||
        (by < y && (by + 1.) > y) ||
        (by > y && (by - 1.) < y)

    List.tryFind (fun (bx, by) ->
        isInHorizontal by && 
            match direction with
            | Left -> bx >= nx - 1. && bx < nx
            | Right -> nx + 1. >= bx && bx > nx)

let tryWalk direction (x, y) worldState =
    let nx = if direction = Left then x - walkSpeed else x + walkSpeed
    let blockers = 
        [
            worldState.blocks |> List.map (fun (bx, by, _) -> (float bx, float by));
            worldState.orcs |> List.filter (fun o -> o.state <> Slain) |> List.map (fun o -> o.position)
        ] |> List.concat
    match checkForHorizontalBlocker (nx, y) direction blockers with
    | Some (bx, _) when direction = Left -> (float bx + 1., y)
    | Some (bx, _) when direction = Right -> (float bx - 1., y)
    | _ -> (nx, y)

let getWalkCommand (runState: RunState) =
    let left = if runState.IsAnyPressed walkLeftKeys then Some Left else None
    let right = if runState.IsAnyPressed walkRightKeys then Some Right else None
    match [left;right] |> List.choose id with
    | [dir] -> Some dir
    | _ -> None

let checkForHorizontalTouch (x, y) direction =
    List.map (fun (x, y) -> float x, float y) 
    >> checkForHorizontalBlocker (x, y) direction
    >> Option.bind (fun (fx, fy) -> Some (int fx, int fy))

let checkForVerticalTouch (x, y) =
    List.map (fun (x, y) -> float x, float y) 
    >> fun list -> [checkForVerticalBlocker (x, y) true list; checkForVerticalBlocker (x, y) false list]
    >> List.tryPick id
    >> Option.bind (fun (fx, fy) -> Some (int fx, int fy))

let processInAir velocity runState worldState = 
    let knight = worldState.knight
    let walkCommand = getWalkCommand runState
    let direction = match walkCommand with Some dir -> dir | None -> knight.direction

    let nv = min (velocity + gravityStrength) terminalVelocity
    let (positionAfterVertical, verticalSpeed) = tryApplyVelocity nv knight.position worldState
    let finalPosition = 
        match walkCommand with 
        | Some dir -> tryWalk dir positionAfterVertical worldState
        | None -> positionAfterVertical

    let hasHitSpikes = checkForVerticalTouch finalPosition worldState.spikes
    let hasHitCoin = checkForVerticalTouch finalPosition worldState.coins
    let newKnight = 
        { knight with 
            position = finalPosition
            direction = direction
            verticalSpeed = verticalSpeed
            score =
                match hasHitCoin with
                | Some _ -> knight.score + coinScore
                | _ -> knight.score
            state = 
                match hasHitSpikes with 
                | Some _ -> Dying runState.elapsed 
                | _ -> Walking }

    let coins = 
        match hasHitCoin with
        | Some c -> List.except [c] worldState.coins
        | _ -> worldState.coins
    { worldState with 
        knight = newKnight
        coins = coins
        events =
            match hasHitCoin, hasHitSpikes with
            | _, Some _ -> HitSpikes::KnightDying::worldState.events
            | Some _, _ -> CoinCollect::worldState.events
            | _ -> worldState.events }

let testForStrickenOrc (kx, ky) direction elapsed (orcs : Orc list) =
    let orc = orcs |> List.tryFind (fun orc -> 
        let ox, oy = orc.position
        oy = ky &&
        (match direction with
        | Left -> ox > kx - 2. && ox < kx
        | Right -> kx < ox && kx + 2. > ox))
    match orc with
    | Some o -> 
        match o.state with
        | Guarding _ -> orcs, Some OrcBlocked
        | _ ->
            orcs |> List.map (fun oi -> 
                match oi with
                | _ when oi = o -> 
                    { o with 
                        health = o.health - 1
                        state = if o.health = 1 then Falling elapsed else o.state }
                | _ -> oi), if o.health = 1 then Some OrcFalling else Some OrcHit
    | _ -> orcs, None

let roughlyEqual (fx, fy) (ix, iy) = 
    abs (fx - float ix) < 0.2 && abs (fy - float iy) < 0.2

let bubbleFromCoin (x, y) = 
    {
        text = sprintf "+ %i pts" coinScore
        startY = float y
        position = float x, float y
    }

let processOnGround (runState: RunState) worldState =
    let knight = worldState.knight
    if strikeKeys |> runState.IsAnyPressed then
        let (newOrcs, event) = 
            testForStrickenOrc 
                knight.position 
                knight.direction 
                runState.elapsed 
                worldState.orcs
        let newKnight = 
            { knight with 
                state = Striking runState.elapsed
                score = match event with | Some OrcFalling -> knight.score + killScore | _ -> knight.score }
        { worldState with 
            knight = newKnight
            orcs = newOrcs
            events = 
                match event with 
                | Some e -> e::KnightSwing::worldState.events 
                | _ -> KnightSwing::worldState.events }
    else 
        let walkCommand = getWalkCommand runState
        let direction = match walkCommand with Some dir -> dir | None -> knight.direction

        if blockKeys |> runState.IsAnyPressed then
            let newKnight = 
                { knight with 
                    direction = direction
                    state = Blocking }
            { worldState with knight = newKnight }
        else if jumpKeys |> runState.WasAnyJustPressed then
            let newKnight = 
                { knight with 
                    direction = direction
                    verticalSpeed = Some jumpSpeed
                    state = Walking }
            { worldState with 
                knight = newKnight
                events = Jump::worldState.events }
        else
            let (position, state) = 
                match walkCommand with
                | Some dir -> tryWalk dir knight.position worldState, Walking
                | None -> knight.position, Standing

            let hasHitCoin = checkForHorizontalTouch knight.position direction worldState.coins
            let hasHitExit = roughlyEqual knight.position worldState.exitPortal
            let newKnight = 
                { knight with 
                    position = position
                    direction = direction
                    state = 
                        match hasHitExit with
                        | true -> WarpingOut runState.elapsed
                        | false -> state
                    score =
                        match hasHitCoin with
                        | Some _ -> knight.score + coinScore
                        | _ -> knight.score }

            let coins, bubbles = 
                match hasHitCoin with
                | Some c -> 
                    let newBubble = bubbleFromCoin c
                    List.except [c] worldState.coins, newBubble::worldState.bubbles
                | _ -> worldState.coins, worldState.bubbles
            { worldState with 
                knight = newKnight
                coins = coins
                bubbles = bubbles
                events =
                    match hasHitCoin, hasHitExit with
                    | _, true -> Warping::worldState.events
                    | Some _, _ -> CoinCollect::worldState.events
                    | _ -> worldState.events }

let processKnight runState worldState =
    let knight = worldState.knight
    match knight.state with
    | WarpingIn t when runState.elapsed - t < warpTime ->
        worldState
    | Dying t when runState.elapsed - t < (animSpeed * float dyingFrames) ->
        worldState
    | Dying _ ->
        { worldState with knight = { worldState.knight with state = Dead } }
    | Striking t when runState.elapsed - t > (animSpeed * float strikeFrames) ->
        let newKnight = { knight with state = Standing }
        { worldState with knight = newKnight }
    | Dead | WarpingOut _ | Striking _ ->
        worldState
    | _ ->
        match knight.verticalSpeed with
        | Some velocity ->
            processInAir velocity runState worldState
        | None ->
            let (_,gravityEffect) = tryApplyVelocity gravityStrength knight.position worldState
            match gravityEffect with
            | Some v ->
                let newKnight = { knight with verticalSpeed = Some v }
                { worldState with knight = newKnight }
            | None ->
                processOnGround runState worldState