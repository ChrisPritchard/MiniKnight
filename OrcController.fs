module OrcController

open Model

let processOrc runState worldState orc =
    // get next pos by direction walk
    // check for edge
    // if so then turn else go
    orc

let processOrcs runState worldState =
    { worldState with orcs = worldState.orcs |> List.map (processOrc runState worldState) }