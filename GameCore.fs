module GameCore

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open Microsoft.Xna.Framework.Input;
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Media

type Loadable =
| Texture of key:string * path:string
| TextureMap of key:string * texturePath:string * keyPath:string
| Font of key:string * path:string
| Sound of key:string * path:string
| Song of key:string * path:string

type Origin = | TopLeft | Centre

type ViewArtifact = 
| Image of assetKey:string * destRect: (int*int*int*int) * color:Color
| MappedImage of assetKey:string * mapKey:string * destRect: (int*int*int*int) * color:Color
| Text of assetKey:string * text:string * position:(int*int) * origin:Origin * scale:float * color:Color
| SoundEffect of string
| Music of string

type Resolution =
| Windowed of int * int
| FullScreen of int * int

type private Content =
| TextureAsset of Texture2D
| TextureMapAsset of Texture2D * Map<string, Rectangle>
| FontAsset of SpriteFont
| SoundAsset of SoundEffect
| MusicAsset of Song

type RunState = {
    elapsed: float
    keyboard: KeyboardInfo
    mouse: MouseInfo
} and KeyboardInfo = {
    pressed: Keys list;
    keysDown: Keys list;
    keysUp: Keys list
} and MouseInfo = {
    position: int * int
    pressed: bool * bool
}
    
type RunState with
    member __.WasJustPressed key = List.contains key __.keyboard.keysDown
    member __.WasAnyJustPressed keyList = keyList |> List.exists __.WasJustPressed
    member __.IsPressed key = List.contains key __.keyboard.pressed
    member __.IsAnyPressed keyList = keyList |> List.exists __.IsPressed

type GameLoop<'TModel> (resolution, assetsToLoad, updateModel, getView)
    as this = 
    inherit Game()

    let mutable graphics = new GraphicsDeviceManager(this)

    let mutable assets = Map.empty<string, Content>

    let mutable keyboardInfo = { pressed = []; keysDown = []; keysUp = [] }
    let mutable currentModel: 'TModel option = None
    let mutable currentView: ViewArtifact list = []
    let mutable currentSong: Song option = None
    let mutable firstDrawComplete = false

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    do 
        match resolution with
        | FullScreen (w,h) -> 
            graphics.PreferredBackBufferWidth <- w
            graphics.PreferredBackBufferHeight <- h
            graphics.IsFullScreen <- true
        | Windowed (w,h) -> 
            graphics.PreferredBackBufferWidth <- w
            graphics.PreferredBackBufferHeight <- h

    let updateKeyboardInfo (keyboard: KeyboardState) (existing: KeyboardInfo) =
        let pressed = keyboard.GetPressedKeys() |> Set.ofArray
        {
            pressed = pressed |> Set.toList
            keysDown = Set.difference pressed (existing.pressed |> Set.ofList) |> Set.toList
            keysUp = Set.difference (existing.pressed |> Set.ofList) pressed |> Set.toList
        }

    let getMouseInfo (mouse: MouseState) =
        {
            position = mouse.X, mouse.Y
            pressed = mouse.LeftButton = ButtonState.Pressed, mouse.RightButton = ButtonState.Pressed
        }

    let asVector2 (x,y) = new Vector2(float32 x, float32 y)
    let asRectangle (x,y,width,height) = 
        new Rectangle (x,y,width,height)
    
    let drawImage (spriteBatch: SpriteBatch) (assetKey, destRect) colour = 
        match Map.tryFind assetKey assets with
        | Some (TextureAsset texture) -> 
            spriteBatch.Draw(
                texture, asRectangle destRect, 
                Unchecked.defaultof<Nullable<Rectangle>>, colour, 0.0f, Vector2.Zero, 
                SpriteEffects.None, 0.0f)
        | None -> sprintf "Missing asset: %s" assetKey |> failwith
        | _-> sprintf "Asset was not a Texture2D: %s" assetKey |> failwith
            
    let drawMappedImage (spriteBatch: SpriteBatch) (assetKey, mapKey, destRect) colour = 
        match Map.tryFind assetKey assets with
        | Some (TextureMapAsset (texture, map)) when map.ContainsKey mapKey -> 
            spriteBatch.Draw(
                texture, asRectangle destRect, 
                map.[mapKey] |> Nullable, colour, 0.0f, Vector2.Zero, 
                SpriteEffects.None, 0.0f)
        | Some (TextureMapAsset _) -> sprintf "Missing map key: %s in asset: %s" mapKey assetKey |> failwith
        | None -> sprintf "Missing asset: %s" assetKey |> failwith
        | _-> sprintf "Asset was not a Texture2D: %s" assetKey |> failwith
    
    let drawText (spriteBatch: SpriteBatch) (assetKey, (text:string), position, origin, scale) colour =
        let font =
            match Map.tryFind assetKey assets with
            | Some (FontAsset f) -> f
            | None -> sprintf "Missing asset: %s" assetKey |> failwith
            | _-> sprintf "Asset was not a SpriteFont: %s" assetKey |> failwith
        let position =
            match origin with
            | TopLeft -> asVector2 position
            | Centre -> 
                let size = Vector2.Divide (font.MeasureString(text), 2.f / float32 scale)
                Vector2.Subtract (asVector2 position, size)
        spriteBatch.DrawString(
            font, text, position, colour, 
            0.0f, Vector2.Zero, float32 scale, SpriteEffects.None, 0.5f)

    let playSound assetKey =
        let sound = 
            match Map.tryFind assetKey assets with
            | Some (SoundAsset s) -> s
            | None -> sprintf "Missing asset: %s" assetKey |> failwith
            | _ -> sprintf "Asset was not a SoundEffect: %s" assetKey |> failwith
        sound.Play () |> ignore

    let playMusic assetKey =
        let song = 
            match Map.tryFind assetKey assets with
            | Some (MusicAsset s) -> s
            | None -> sprintf "Missing asset: %s" assetKey |> failwith
            | _ -> sprintf "Asset was not a Song: %s" assetKey |> failwith
        match currentSong with
        | Some s when s = song -> ()
        | _ ->
            currentSong <- Some song
            MediaPlayer.Play (song)
            MediaPlayer.IsRepeating <- true

    override __.LoadContent() = 
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        assets <- 
            assetsToLoad
            |> List.map (
                function
                | Texture (key, path) -> 
                    use stream = File.OpenRead path
                    key, Texture2D.FromStream (this.GraphicsDevice, stream) |> TextureAsset
                | TextureMap (key, texturePath, keyPath) -> 
                    use stream = File.OpenRead texturePath
                    let texture = Texture2D.FromStream (this.GraphicsDevice, stream)
                    let content = 
                        File.ReadAllLines keyPath |> Seq.skip 1 
                        |> Seq.map (fun line -> line.Split(',') |> fun s -> s.[0], new Rectangle(int s.[1], int s.[2], int s.[3], int s.[4]))
                        |> Map.ofSeq
                    key, TextureMapAsset (texture, content)
                | Font (key, path) -> 
                    key, this.Content.Load<SpriteFont> path |> FontAsset
                | Sound (key, path) -> 
                    use stream = File.OpenRead path
                    key, SoundEffect.FromStream stream |> SoundAsset
                | Song (key, path) ->
                    let uri = new Uri (path, UriKind.RelativeOrAbsolute)
                    key, Song.FromUri (key, uri) |> MusicAsset) 
            |> Map.ofList

    override __.Update(gameTime) =
        keyboardInfo <- updateKeyboardInfo (Keyboard.GetState()) keyboardInfo
        let mouseInfo = getMouseInfo (Mouse.GetState())
        let runState = { 
            elapsed = gameTime.TotalGameTime.TotalMilliseconds 
            keyboard = keyboardInfo
            mouse = mouseInfo
        }
        
        match currentModel with
        | None -> 
            currentModel <- updateModel runState currentModel
        | Some _ when firstDrawComplete ->
            currentModel <- updateModel runState currentModel
        | _ -> ()
            
        match currentModel with
        | None -> __.Exit()
        | Some model ->
            currentView <- getView runState model

    override __.Draw(_) =
        firstDrawComplete <- true
        this.GraphicsDevice.Clear Color.Black
        
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp)

        currentView
            |> List.iter (
                function 
                | Image (a,d,c) -> drawImage spriteBatch (a,d) c
                | MappedImage (a,m,d,c) -> drawMappedImage spriteBatch (a,m,d) c
                | Text (a,t,p,o,s,c) -> drawText spriteBatch (a,t,p,o,s) c
                | SoundEffect s -> playSound s
                | Music s -> playMusic s)

        spriteBatch.End()