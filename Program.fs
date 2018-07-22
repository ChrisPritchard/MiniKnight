
[<EntryPoint>]
let main _ =
    use game = new GameCore.GameLoop<Model.GameModel>(View.resolution, View.assetsToLoad, GameController.advanceGame, View.getView)
    game.Run ()
    0