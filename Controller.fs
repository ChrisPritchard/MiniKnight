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

type Command = | WalkLeft | WalkRight | Jump | Strike | Block
let commandMap = [
    ([Keys.A;Keys.Left], WalkLeft)
    ([Keys.D;Keys.Right], WalkRight)
    ([Keys.W;Keys.Space], Jump)
    ([Keys.LeftControl;Keys.RightControl], Strike)
    ([Keys.LeftAlt;Keys.RightAlt], Block)
]

let getCommands (runState: RunState) = 
    commandMap 
        |> List.map (fun (keys, command) ->
            if List.exists runState.IsPressed keys then Some command else None)
        |> List.choose id

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

let checkForJumpAndFall commands blocks (knightDirection, knightPosition) =
    (knightDirection, knightPosition)

let checkForWalk commands blocks (knightDirection, knightPosition) =
    if List.contains WalkLeft commands then Left, tryWalk Left knightPosition blocks
    else if List.contains WalkRight commands then Right, tryWalk Right knightPosition blocks
    else knightDirection, knightPosition

let processKnight runState (worldState, controllerState) =
    let knight = worldState.knight
    let commands = getCommands runState

    let (newDirection, newPosition) = 
        (knight.direction, knight.position)
        |> checkForJumpAndFall commands worldState.blocks 
        |> checkForWalk commands worldState.blocks 

    // if airborne then rise/fall
        // check collision
    // if striking continue
    // if blocking block
    // gather commands: walking/jumping or striking
        // check collision
    let newKnight = 
        { knight with
            direction = newDirection
            position = newPosition }
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