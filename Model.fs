module Model

type World = 
    | Menu
    | Playing of PlayingState
    | GameOver of score:int
and PlayingState = {
    level: (int * int * MapTile) list
    coins: (int * int) list
    orcs: Orc list
    knight: Knight
} 
and MapTile = | Block | Spikes | Orc | Coin | EntryPortal | ExitPortal | Knight
and Orc = {
    position: float * float
    state: EntityState
    direction: Direction
    health: int
}
and EntityState = 
    | Standing | Walking | Jumping of velocity:float 
    | Striking | Blocking | Hit of startTime:float | Dead
and Direction = | Left | Right
and Knight = {
    position: float * float
    state: EntityState
    direction: Direction
    health: int
    score: int
}

let startWorld = Playing { 
    level = []
    coins = []
    orcs = []
    knight = 
    {
        position = 0.,0.
        state = Standing
        direction = Right
        health = 3
        score = 0
    } 
}