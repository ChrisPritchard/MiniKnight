module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeForStrikes = 400.
//let timeBetweenMovement = 25.
//let timeBetweenGravity = 25.
let walkSpeed = 0.15
let jumpSpeed = -0.55
let gravityStrength = 0.05
let terminalVelocity = 0.9

let walkLeftKeys = [Keys.A;Keys.Left]
let walkRightKeys = [Keys.D;Keys.Right]
let jumpKeys = [Keys.W;Keys.Space]
let strikeKeys = [Keys.LeftControl;Keys.RightControl]
let blockKeys = [Keys.LeftAlt;Keys.RightAlt]

let tryApplyVelocity verticalSpeed (x, y) blocks =
    let (nx, ny) = (x, y + verticalSpeed)
    let (floorx, ceilx, floory, ceily) = 
        int (floor nx), int (ceil nx), int (floor ny), int (ceil ny)

    if verticalSpeed < 0. then
        let ceiling = blocks |> List.tryFind (fun (x, y, _) ->
            (x = floorx || x = ceilx) && 
            y = ceily - 1)
        match ceiling with
        | None -> (nx, ny), Some verticalSpeed
        | Some (_, y, _) -> (nx, float y + 1.), Some 0.
    else
        let floor = blocks |> List.tryFind (fun (x, y, _) ->
            (x = floorx || x = ceilx) && 
            y = floory + 1)
        match floor with
        | None -> (nx, ny), Some verticalSpeed
        | Some (_, y, _) -> (nx, float y - 1.), None

let tryWalk direction (x, y) blocks =
    let newX = if direction = Left then x - walkSpeed else x + walkSpeed
    let blocker = blocks |> List.tryFind (fun (bx, by, _) -> 
        float by = y && (if direction = Left then float bx = floor x else float bx = ceil x))
    match blocker with Some _ -> (x, y) | None -> (newX, y)

let getWalkCommand (runState: RunState) =
    let left = if runState.IsAnyPressed walkLeftKeys then Some Left else None
    let right = if runState.IsAnyPressed walkRightKeys then Some Right else None
    match [left;right] |> List.choose id with
    | [dir] -> Some dir
    | _ -> None

let canStrike runState controllerState = 
    runState.elapsed - controllerState.lastStrikeTime >= timeForStrikes

let isStriking knight runState controllerState =
    knight.state = Striking && not <| canStrike runState controllerState

let processKnight runState (worldState, controllerState) =
    let knight = worldState.knight
    let noChange = (worldState, controllerState)

    let walkCommand = getWalkCommand runState
    let direction = match walkCommand with Some dir -> dir | None -> knight.direction

    match knight.verticalSpeed with
    | Some v ->
        let nv = min (v + gravityStrength) terminalVelocity
        let (positionAfterVertical, verticalSpeed) = tryApplyVelocity nv knight.position worldState.blocks
        let finalPosition = 
            match walkCommand with 
            | Some dir -> tryWalk dir positionAfterVertical worldState.blocks
            | None -> positionAfterVertical

        let newKnight = 
            { knight with 
                position = finalPosition
                direction = direction
                verticalSpeed = verticalSpeed
                state = Walking }

        { worldState with knight = newKnight }, controllerState
    | None ->
        let (_,gravityEffect) = tryApplyVelocity gravityStrength knight.position worldState.blocks
        match gravityEffect with
        | Some v ->
            let newKnight = 
                { knight with 
                    position = knight.position
                    direction = direction
                    verticalSpeed = Some v
                    state = Walking }
            { worldState with knight = newKnight }, controllerState
        | None ->
            if isStriking knight runState controllerState then
                noChange
            else if strikeKeys |> runState.IsAnyPressed && canStrike runState controllerState then
                let newKnight = 
                    { knight with 
                        direction = direction
                        state = Striking }
                { worldState with knight = newKnight }, { controllerState with lastStrikeTime = runState.elapsed }
            else if blockKeys |> runState.IsAnyPressed then
                let newKnight = 
                    { knight with 
                        direction = direction
                        state = Blocking }
                { worldState with knight = newKnight }, controllerState
            else if jumpKeys |> runState.WasAnyJustPressed then
                let newKnight = 
                    { knight with 
                        direction = direction
                        verticalSpeed = Some jumpSpeed
                        state = Walking }
                { worldState with knight = newKnight }, controllerState
            else
                let (position, state) = 
                    match walkCommand with
                    | Some dir -> tryWalk dir knight.position worldState.blocks, Walking
                    | None -> knight.position, Standing

                let newKnight = 
                    { knight with 
                        position = position
                        direction = direction
                        state = state }

                { worldState with knight = newKnight }, controllerState

let handlePlayingState runState worldState controllerState =
    (worldState, controllerState)
    |> processKnight runState
    |> Playing |> Some

let advanceGame (runState : RunState) =
    function
    | None -> MapLoader.getLevel 1 |> getLevelModel |> Some 
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some model -> 
        match model with
        | Playing (worldState, controllerState) -> 
            handlePlayingState runState worldState controllerState
        | _ -> Some model