module MapLoader

open System.Drawing
open Model

let mapKey = [
    (Color.FromArgb(128, 128, 128), Block)
    (Color.FromArgb(255, 0, 0), Spikes)
    (Color.FromArgb(38, 127, 0), EntryPortal)
    (Color.FromArgb(10, 38, 255), ExitPortal)
    (Color.FromArgb(255, 216, 0), Coin)
    (Color.FromArgb(0, 0, 0), Orc)
]

let getLevel num = 
    use bitmap = Bitmap.FromFile <| sprintf "./Content/Maps/map%i.bmp" num :?> Bitmap
    [0..bitmap.Height-1] |> Seq.collect (fun y -> [0..bitmap.Width-1] |> Seq.map (fun x ->
        let color = bitmap.GetPixel (x, y)
        match Seq.tryFind (fun (c,_) -> c = color) mapKey with
        | Some t -> Some (x, y, t |> snd)
        | _ -> None)) |> Seq.choose id |> Seq.toList