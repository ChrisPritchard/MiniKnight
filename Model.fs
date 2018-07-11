module Model

type World = {
    state: GameState
} and GameState = | Title

let startWorld = { state = Title }