module Controller

open Model

let advanceGame runState =
    function
    | None -> Some startWorld
    | Some world -> Some world
