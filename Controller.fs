module Controller

open GameCore
open Model
open Microsoft.Xna.Framework.Input

let timeBetweenCommands = 100.
let timeForAttacks = 200.
let timeBetweenMovement = 25.
let timeBetweenGravity = 25.
let walkSpeed = 0.15
let jumpSpeed = -0.55
let gravityStrength = 0.05
let terminalSpeed = 0.9

let walkLeftKeys = [Keys.A;Keys.Left]
let walkRightKeys = [Keys.D;Keys.Right]
let jumpKeys = [Keys.W;Keys.Space]
let strikeKeys = [Keys.LeftControl;Keys.RightControl]
let blockKeys = [Keys.LeftAlt;Keys.RightAlt]

// type Command = | WalkLeft | WalkRight | Jump | Strike | Block
// let commandMap = [
//     ([Keys.A;Keys.Left], WalkLeft)
//     ([Keys.D;Keys.Right], WalkRight)
//     ([Keys.W;Keys.Space], Jump)
//     ([Keys.LeftControl;Keys.RightControl], Strike)
//     ([Keys.LeftAlt;Keys.RightAlt], Block)
// ]

// let getCommands (runState: RunState) = 
//     commandMap 
//         |> List.map (fun (keys, command) ->
//             if List.exists runState.IsPressed keys then Some command else None)
//         |> List.choose id

// let horizontalCollision direction (x, y) blocks =
//     let dx = if direction = Left then floor x |> int else ceil x |> int
//     blocks |> List.exists (fun (mx, my, _) -> my = int y && mx = dx) 
    
// let verticalCollision (x, y) blocks =
//     let fy, cy = floor y |> int, ceil y |> int
//     blocks |> List.exists (fun (mx, my, _) -> mx = int x && (my = fy || my = cy)) 

// let checkForFallingPosChange (runState: RunState) (worldState, controllerState) =
//     if runState.elapsed - controllerState.lastGravityTime < timeBetweenGravity then
//         worldState,
//         controllerState
//     else
//         let knight = worldState.knight;
//         let newFallSpeed = knight.fallSpeed + gravityStrength
//         let newFallSpeed = if abs newFallSpeed > terminalSpeed then knight.fallSpeed else newFallSpeed
//         let (x, y) = knight.position;
//         let newY = y + newFallSpeed;
//         let isCollision = verticalCollision (x, newY) worldState.blocks
//         let newPos = if isCollision then (x, y) else (x, newY)
//         let newFallSpeed = if isCollision then 0. else newFallSpeed
//         (worldState.withKnightPosition newPos).withKnightFallSpeed newFallSpeed,
//         { controllerState with lastGravityTime = runState.elapsed }

// let checkForDirectionChange (runState: RunState) (worldState, controllerState) =
//     let justPressed = List.exists runState.WasJustPressed
//     let current = worldState.knight.direction

//     if runState.elapsed - controllerState.lastCommandTime < timeBetweenCommands 
//     then 
//         worldState, 
//         controllerState
//     else if justPressed [Keys.Left; Keys.A] && current <> Left 
//     then
//         worldState.withKnightDirection Left,
//         { controllerState with lastCommandTime = runState.elapsed }
//     else if justPressed [Keys.Right; Keys.D] && current <> Right 
//     then
//         worldState.withKnightDirection Right,
//         { controllerState with lastCommandTime = runState.elapsed }
//     else
//         worldState, 
//         controllerState

// let checkForStateChange runState (worldState, controllerState) =
//     let anyPressed = List.exists (fun k -> List.contains k runState.keyboard.pressed)
//     let canCommand = runState.elapsed - controllerState.lastCommandTime >= timeBetweenCommands

//     if worldState.knight.state = Striking && runState.elapsed - controllerState.lastAttackTime < timeForAttacks 
//     then
//         worldState, 
//         controllerState
//     else if canCommand && (runState.WasJustPressed Keys.LeftControl || runState.WasJustPressed Keys.RightControl) 
//     then
//         worldState.withKnightState Striking,
//         { controllerState with
//             lastCommandTime = runState.elapsed
//             lastAttackTime = runState.elapsed }
//     else if canCommand && worldState.knight.fallSpeed = 0. && (runState.WasJustPressed Keys.Space || runState.WasJustPressed Keys.W) 
//     then
//         worldState.withKnightFallSpeed jumpSpeed,
//         { controllerState with
//             lastCommandTime = runState.elapsed }
//     else if anyPressed [Keys.LeftAlt;Keys.RightAlt] 
//     then 
//         worldState.withKnightState Blocking,
//         controllerState
//     else if anyPressed [Keys.Left;Keys.A;Keys.D;Keys.Right] 
//     then 
//         worldState.withKnightState Walking,
//         controllerState
//     else
//         worldState.withKnightState Standing,
//         controllerState

// let checkForWalkingPosChange runState (worldState, controllerState) =
//     let knight = worldState.knight
//     if runState.elapsed - controllerState.lastMovementTime < timeBetweenMovement then
//         worldState,
//         controllerState
//     else
//         let (x, y) = knight.position
//         match knight.state with
//         | Walking -> 
//             let newX = if knight.direction = Left then x - walkSpeed else x + walkSpeed
//             let newPos = if horizontalCollision knight.direction (newX, y) worldState.blocks then (x, y) else (newX, y)
//             worldState.withKnightPosition newPos,
//             { controllerState with lastMovementTime = runState.elapsed }
//         | _ -> 
//             worldState,
//             controllerState

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

let isStriking knight runState controllerState =
    knight.state = Striking && runState.elapsed - controllerState.lastAttackTime < timeForAttacks

let processKnight runState (worldState, controllerState) =
    let knight = worldState.knight
    let noChange = (worldState, controllerState)

    let walkCommand = getWalkCommand runState
    let direction = match walkCommand with Some dir -> dir | None -> knight.direction

    match knight.verticalSpeed with
    | Some v ->
        // TODO apply Gravity
        let position = 
            match walkCommand with 
            | Some dir -> tryWalk dir knight.position worldState.blocks
            | None -> knight.position

        let newKnight = 
            { knight with 
                position = position
                direction = direction
                state = Walking }

        { worldState with knight = newKnight }, controllerState
    | None ->
        if isStriking knight runState controllerState then
            noChange
        else
            // TODO check for new strike, jump or block
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
    // |> checkForFallingPosChange runState
    // |> checkForDirectionChange runState
    // |> checkForStateChange runState
    // |> checkForWalkingPosChange runState
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