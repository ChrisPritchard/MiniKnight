# MiniKnight

A pixel-art platformer where you must fight your way to the portal, collecting coins and slaying orcs along the way! Features five levels (each loaded from pixel bitmaps), sound effects, music, enemy AI etc. Somewhat difficult, but quite forgiving. Also includes high score functionality.

<p align="center">
    <img alt="demo capture gif" src="./screencapture.gif">
</p>

Coded in __F#__ on dotnet core 2.1. Developed using VS Code 1.25.1 on Windows 10. Game loop engine is MonoGame 3.7

> **Update:** Now in .NET 5, still with VS Code, and on OSX as well as Win 10.

__To run:__

- Ensure you have the .NET 5 sdk installed from here: <https://www.microsoft.com/net/download>
- From the base directory of the repo, run the command __dotnet run__
- To compile an exe (or platform equivalent) use __dotnet build -r [rid]__ where [rid] is the appropriate runtime identifier from here: <https://docs.microsoft.com/en-us/dotnet/core/rid-catalog>

## Acknowledgements

All code is my own, and available under MIT. I am using the following additional assets, all sourced from <https://opengameart.org>:

- The Knight and Orc frames: <https://opengameart.org/content/mini-knight>
- The stone floors and spikes: <https://opengameart.org/content/castle-platformer>
- All the sound effects: <https://opengameart.org/content/rpg-sound-pack>
- And the music (which is excellent): <https://opengameart.org/content/8-bit-explorer-theme>

Additionally, I used ScreenToGif to create the screencapture: <https://github.com/NickeManarin/ScreenToGif>

And the __coders crux__ font: <https://www.dafont.com/coders-crux.font>

## Supported platforms

Being .NET 5, it should work on all platforms that supports (Windows, Linux, Mac). Tested (and largely coded on) Windows 10, and on OSX. A full list of dotnet core supported platforms can be found from here: <https://github.com/dotnet/core/blob/master/release-notes/2.1/2.1-supported-os.md>

The project relies on MonoGame and LibGDI (for bitmap loading) - both have been added as nuget packages and tested on OSX, but I am still testing this for other platforms. If this causes an issue, try installing the monogame sdk and libgdi from your package manager etc.

## Note on development sequence

This project was developed after __Tetris__ [here](https://github.com/ChrisPritchard/Tetris).

The next project developed after this, and using the lessons learned, was __DungeonRaider__ [here](https://github.com/ChrisPritchard/DungeonRaider).