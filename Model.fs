module Model

type World = {
    state: GameState
    level: MapTile list
    coins: (int * int) list
    orcs: Orc list
    knight: Knight
} 
and GameState = | Title | Playing | GameOver
and MapTile = | Block | Spikes | Orc | Coin | EntryPortal | ExitPortal | Knight
and Orc = {
    position: int * int
    state: EntityState
    direction: Direction
    health: int
}
and EntityState = | Walking | Jumping | Standing | Striking | Blocking | Hit | Dead
and Direction = | Left | Right
and Knight = {
    position: int * int
    state: EntityState
    direction: Direction
    health: int
    score: int
}

let startWorld = { 
    state = Playing
    level = []
    coins = []
    orcs = []
    knight = {
        position = 0,0
        state = Standing
        direction = Right
        health = 3
        score = 0
    } 
}