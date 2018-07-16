module Model

type GameModel = 
    | Menu
    | Playing of WorldState * ControllerState
    | GameOver of score:int
and WorldState = {
    blocks: (int * int * string) list
    spikes: (int * int) list
    coins: (int * int) list
    entryPortal: int * int
    exitPortal: int * int
    orcs: Orc list
    knight: Knight
} 
and MapTile = | Block | Spikes | Coin | Orc | EntryPortal | ExitPortal
and Orc = {
    position: float * float
    state: EntityState
    direction: Direction
    health: int
}
and EntityState = 
    | Standing | Walking | Jumping of velocity:(float * float)
    | Striking | Blocking | Hit of startTime:float
    | Dying | Dead
and Direction = | Left | Right
and Knight = {
    position: float * float
    state: EntityState
    direction: Direction
    health: int
    score: int
}
and ControllerState = { 
    lastCommandTime:float
    lastAttackTime:float
    lastPhysicsTime:float 
}

type WorldState with 
    member __.withKnightDirection direction = { __ with knight = { __.knight with direction = direction } }
    member __.withKnightPosition position = { __ with knight = { __.knight with position = position } }
    member __.withKnightState state = { __ with knight = { __.knight with state = state } }

let getStartModel level = 
    Playing 
    <| (
        { 
            blocks = []
            spikes = []
            coins = []
            orcs = []
            entryPortal = 0,0
            exitPortal = 0,0
            knight = 
            {
                position = 
                    match Seq.tryFind (fun (_,_,kind) -> kind = EntryPortal) level with 
                    | Some (x,y,_) -> float x, float y 
                    | None -> 0.,0.
                state = Walking
                direction = Right
                health = 3
                score = 0
            }
        },
        { lastCommandTime = 0.; lastAttackTime = 0.; lastPhysicsTime = 0. }
    )