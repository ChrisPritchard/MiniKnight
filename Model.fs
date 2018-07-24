module Model

type GameModel = 
    | Menu
    | Playing of WorldState
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
    | Standing | Walking | Striking of startTime:float | Blocking 
    | Hit of startTime:float | Dying of startTime:float | Dead
    | WarpingIn of startTime:float | WarpingOut of startTime:float
and Direction = | Left | Right
and Knight = {
    position: float * float
    state: EntityState
    direction: Direction
    verticalSpeed: float option
    health: int
    score: int
}

let validAdjacents = 
    [
        "00111000";"00111110";"00001110";"00001000";"11111000";"11111111";"10001111";"10001000";
        "11100000";"11100011";"10000011";"10000000";"00100000";"00100010";"00000010";"00000000"
    ]

let adjacencyKey (x, y) blocks = 
    let adjacent = 
        blocks 
        |> Seq.filter (fun (ox,oy) -> abs (ox - x) < 2 && abs (oy - y) < 2) 
        |> Seq.map (fun (ox, oy) -> (ox - x, oy - y))
    let key = 
        [(0, -1);(1, -1);(1, 0);(1, 1);(0, 1);(-1, 1);(-1, 0);(-1, -1)]
        |> Seq.map (fun pos -> if Seq.contains pos adjacent then "1" else "0")
        |> String.concat ""
    if List.contains key validAdjacents then key else "00000000"

let getLevelModel levelMapTiles = 
    let byKind = Seq.groupBy (fun (_, _, kind) -> kind) levelMapTiles |> Map.ofSeq
    let ofKind k = 
        match Map.tryFind k byKind with 
        | Some o -> Seq.map (fun (x, y, _) -> x, y) o |> Seq.toList
        | _ -> []
    let oneKind k ifNone = 
        ofKind k |> List.tryHead |> function | Some o -> o | _ -> ifNone
    
    let blocks = ofKind Block
    let adjacencyMapped = blocks |> List.map (fun (x, y) -> 
        x, y, adjacencyKey (x, y) blocks)
    
    let entryPortal = oneKind EntryPortal (0,0)
    Playing { 
        blocks = adjacencyMapped
        spikes = ofKind Spikes
        coins = ofKind Coin
        orcs = []
        entryPortal = entryPortal
        exitPortal = oneKind ExitPortal (0,0)
        knight = 
        {
            position = entryPortal |> (fun (x, y) -> float x, float y)
            state = WarpingIn 0.
            direction = Right
            verticalSpeed = None
            health = 3
            score = 0
        }
    }