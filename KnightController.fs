module KnightController

open GameCore
open Model
open View
open CollisionDetection
open Microsoft.Xna.Framework.Input

let walkSpeed = 0.15
let jumpSpeed = -0.55
let gravityStrength = 0.05
let terminalVelocity = 0.9

let coinScore = 2

let walkLeftKeys = [Keys.A;Keys.Left]
let walkRightKeys = [Keys.D;Keys.Right]
let jumpKeys = [Keys.W;Keys.Space]
let strikeKeys = [Keys.LeftControl;Keys.RightControl]
let blockKeys = [Keys.LeftAlt;Keys.RightAlt]

let tryApplyVelocity verticalSpeed (x, y) blocks =
    let ny = y + verticalSpeed
    let blocks = blocks |> List.map (fun (bx, by, _) -> (bx, by))
    if verticalSpeed < 0. then
        let ceiling = tryFindCollision (x, ny) blocks North
        match ceiling with
        | Some (_, by) -> (x, float by + 1.), Some 0.
        | None -> (x, ny), Some verticalSpeed
    else
        let floor = tryFindCollision (x, ny) blocks South
        match floor with
        | Some (_, by) -> (x, float by - 1.), None
        | None -> (x, ny), Some verticalSpeed

let tryWalk direction (x, y) blocks =
    let nx = if direction = Left then x - walkSpeed else x + walkSpeed
    let blocks = blocks |> List.map (fun (bx, by, _) -> (bx, by))
    if direction = Left then
        let wall = tryFindCollision (nx, y) blocks West
        match wall with
        | Some (bx, _) -> (float bx + 1., y)
        | None -> (nx, y)
    else
        let wall = tryFindCollision (nx, y) blocks East
        match wall with
        | Some (bx, _) -> (float bx - 1., y)
        | None -> (nx, y)

let getWalkCommand (runState: RunState) =
    let left = if runState.IsAnyPressed walkLeftKeys then Some Left else None
    let right = if runState.IsAnyPressed walkRightKeys then Some Right else None
    match [left;right] |> List.choose id with
    | [dir] -> Some dir
    | _ -> None

let processInAir velocity runState worldState = 
    let knight = worldState.knight
    let walkCommand = getWalkCommand runState
    let direction = match walkCommand with Some dir -> dir | None -> knight.direction

    let nv = min (velocity + gravityStrength) terminalVelocity
    let (positionAfterVertical, verticalSpeed) = tryApplyVelocity nv knight.position worldState.blocks
    let finalPosition = 
        match walkCommand with 
        | Some dir -> tryWalk dir positionAfterVertical worldState.blocks
        | None -> positionAfterVertical

    let hasHitSpikes = [North;South] |> List.tryPick (tryFindCollision finalPosition worldState.spikes)
    let hasHitCoin = [North;South] |> List.tryPick (tryFindCollision finalPosition worldState.coins)
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
    { worldState with knight = newKnight; coins = coins }

let roughlyEqual (fx, fy) (ix, iy) = 
    abs (fx - float ix) < 0.2 && abs (fy - float iy) < 0.2

let processOnGround (runState: RunState) worldState =
    let knight = worldState.knight
    if strikeKeys |> runState.IsAnyPressed then
        let newKnight = { knight with  state = Striking runState.elapsed }
        { worldState with knight = newKnight }
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
            { worldState with knight = newKnight }
        else
            let (position, state) = 
                match walkCommand with
                | Some dir -> tryWalk dir knight.position worldState.blocks, Walking
                | None -> knight.position, Standing

            let coinDir = match direction with Left -> West | _ -> East
            let hasHitCoin = tryFindCollision knight.position worldState.coins coinDir
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

            let coins = 
                match hasHitCoin with
                | Some c -> List.except [c] worldState.coins
                | _ -> worldState.coins
            { worldState with knight = newKnight; coins = coins }

let processKnight runState worldState =
    let knight = worldState.knight
    match knight.state with
    | WarpingIn startTime when runState.elapsed - startTime < warpTime ->
        worldState
    | Dead | WarpingOut _ ->
        worldState
    | Dying startTime when runState.elapsed - startTime < (animSpeed * float dyingFrames) ->
        worldState
    | Dying _ ->
        { worldState with knight = { worldState.knight with state = Dead } }
    | Striking startTime when runState.elapsed - startTime < (animSpeed * float strikeFrames) ->
        worldState
    | Striking _ ->
        let newKnight = { knight with state = Standing }
        { worldState with knight = newKnight }
    | _ ->
        match knight.verticalSpeed with
        | Some velocity ->
            processInAir velocity runState worldState
        | None ->
            let (_,gravityEffect) = tryApplyVelocity gravityStrength knight.position worldState.blocks
            match gravityEffect with
            | Some v ->
                let newKnight = { knight with verticalSpeed = Some v }
                { worldState with knight = newKnight }
            | None ->
                processOnGround runState worldState