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

let startWorld = Playing { 
    level = [ 
        (-3,0,Spikes);                                                                    (3,0,Spikes);
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
}