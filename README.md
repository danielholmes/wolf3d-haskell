# Wolfenstein 3D in Haskell

![Build](https://github.com/danielholmes/wolf3d-haskell/workflows/Build/badge.svg)

Wolfenstein 3D in Haskell. Inspired by [John Carmack's Quakecon 2013 Talk](https://youtu.be/Uooh0Y9fC_M?t=4660) and
mentions of trying to implement games (and Wolf3D in particular) in functional programming/Haskell.

The implementation aims to use similar runtime data structures to the original source. I want to get a better 
understanding of how the original works and also reap the performance rewards (my naive attempts at a ray caster before
performed badly). Also I hope this helps getting the same feel as the original.

This is a work in progress and is still in early stages.


## Requirements

 - [Stack](https://www.haskellstack.org) (tested with 1.9.1-1.9.3)
 - [SDL2](https://www.libsdl.org/) (tested with 2.0.8-2.0.9)
 - [SDL2 Image](https://www.libsdl.org/projects/SDL_image/) (tested with 2.0.4)
 - [SDL2 TTF](https://www.libsdl.org/projects/SDL_ttf/) (tested with 2.0.14)


## Setting up Development Environment

```bash
stack setup
stack build --pedantic
```


## Running

```bash
stack exec wolf3d
```

or


```bash
stack exec wolf3d-debug
```


## Running REPL

```bash
stack repl
```


## Running Tests

```bash
stack test --pedantic
```

Or continuous:

```bash
stack test --pedantic --file-watch
```


## TODO
 
 - use data.array for worldmap instead - better performance accessing by index
 
 - physics engine (2d top down). wolfs is simple
   - general
         1. Move x and y, if okay then return
         2. move x only, if okay return
         3. move y only, if oka return
         4. return
   - functional structure
         - input a bunch of force events (vector2 + character)
         - normalise to one force event per char
         - create move to events which result from forces
             - which apply the force and take in to account any bumping up against
             - items should be static (cant be moved), dynamic (can be moved), virtual (move right through them)
         - collision events between 2
 
 - have press m to toggle map for debug
 - move sprites forward a bit. This was fudged in original:
   - https://github.com/id-Software/wolf3d/blob/05167784ef009d0d0daefe8d012b027f39dc8541/WOLFSRC/WL_DRAW.C#L227
 - id system for items
 - multi engine approach outlined in first 60 or so slides of https://www.slideshare.net/naughty_dog/multiprocessor-game-loops-lessons-from-uncharted-2-among-thieves
 - static sprite targets that hero can shoot and kill
 - SimItem infrastructure - Update each item to produce events, process those events which generate new events
 - Split SDL dependency into own module
   - split Display into multiple modules
   - merge UI into display? - simplifies some things like disposing and setting up renderer
 
 - rendering alternatives
   - https://github.com/bkaradzic/bgfx looks good for rendering
   - OpenGL renderer - https://hackage.haskell.org/package/OpenGL
   - See https://github.com/jxv/sdl2-fps
   - SoftwareRenderer provides much better performance (in createRenderer)
   - try using SDL.opengl ?
   - do some research on spritesheets, is it faster?
     - abstract textures into class - TextureSource, AnimatedTextureSource, etc.
 
 - Hold multiple weapons and change between them with key presses
 - read levels from shareware files (see https://devinsmith.net/backups/bruce/wolf3d.html)
 - shoot Uzi
 - animate Uzi
 - difference for pistol and uzi - auto vs semi auto
 - Structure cabal project in such a way that modules not exposed to main can still be exposed to test
   - Do general deeper research into cabal project and possibilities - multiple libraries?
 - Split Wolf3D.Sim into separate files, resolving circular dependencies
   - https://stackoverflow.com/questions/8650297/haskell-recursive-circular-module-definitions
   - https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/separate-compilation.html#mutual-recursion 
 - Dont render hidden sprites
   - only render within field of view bounds
   - only render in front of walls
 - sprite items stop hero moving movement
 - pickup items
 - doors
 - enemy
 - secret doors
 - All events together at the end of game loop passed to audio handler, which plays audio
 - mouse look horizontal
 - mouse look up/down (move projection plane up/down)
 - SDL dispose textures at the end
 - Example world that shows off diagonal walls, etc
 - Console menu system at start


## Nice to haves

 - better index for weapon animation data
 - Optimise render - a lot of calculations can be done once
 - Try reading original data file? See carmackExpand in wolf3d-html. Or maybe just use the format from wolf3d-html
