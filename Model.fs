module Model

type GameModel = 
    | Menu
    | Playing of WorldState * ControllerState
    | GameOver of score:int
and WorldState = {
    level: (int * int * MapTile) list
    coins: (int * int) list
    orcs: Orc list
    knight: Knight
} 
and MapTile = | Block | Spikes | Coin | EntryPortal | ExitPortal
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

let startModel = 
    Playing 
    <| (
        { 
            level = [
                    (-3,0,Block);(-2,0,EntryPortal);(-1,0,Coin);(-0,0,Coin);(1,0,Coin);(2,0,ExitPortal);(3,0,Block);
                    (-3,1,Spikes); (-2,1,Block); (-1,1,Block); (0,1,Block); (1,1,Block); (2,1,Block); (3,1,Spikes) 
                ]
            coins = []
            orcs = []
            knight = 
            {
                position = 0.,0.
                state = Walking
                direction = Right
                health = 3
                score = 0
            }
        },
        { lastCommandTime = 0.; lastAttackTime = 0.; lastPhysicsTime = 0. }
    )