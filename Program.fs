
[<EntryPoint>]
let main _ =
    use game = new GameCore.GameLoop<Model.GameModel>(View.resolution, View.assetsToLoad, Controller.advanceGame, View.getView)
    game.Run ()
    0