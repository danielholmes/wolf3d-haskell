# Wolfenstein 3D in Haskell

[![Build Status](https://travis-ci.org/danielholmes/wolf3d-haskell.svg?branch=master)](https://travis-ci.org/danielholmes/wolf3d-haskell)

Wolfenstein 3D engine implementation in Haskell. Functionality is equivalent, but doesn't use same types of 
optimisations. Inspired by [John Carmack's Quakecon 2013 Talk](https://youtu.be/Uooh0Y9fC_M?t=4660)


## Requirements

 - [Stack](https://www.haskellstack.org) (Tested with 1.4)
 - [SDL2 2.0.4+](https://www.libsdl.org/)
 - [SDL2 Image](https://www.libsdl.org/projects/SDL_image/)
 - [SDL2 TTF](https://www.libsdl.org/projects/SDL_ttf/)


## Setting up Development Environment

```bash
stack setup
stack build --pedantic
```


## Running

```bash
stack exec wolf3d
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
 
 - physics engine (2d top down).  wolfs is simple
     1. Move x and y, if okay then return
     2. move x only, if okay return
     3. move y only, if oka return
     4. return
 - Pickup item
 - move sprites forward a bit. This was fudged in original:
   - https://github.com/id-Software/wolf3d/blob/05167784ef009d0d0daefe8d012b027f39dc8541/WOLFSRC/WL_DRAW.C#L227
 - id system for items
 - static sprite targets that can shoot and kill
 - SimItem infrastructure - Update each item to produce events, process those events which generate new events
 - Split SDL dependency into own module
   - split Display into multiple modules
   - merge UI into display? - simplifies some things like disposing and setting up renderer
 
 - optimise rendering
   - SoftwareRenderer provides much better performance (in createRenderer)
   - try using SDL.opengl ?
   - rendering solid colur instead of textures improves from ~30ms - ~5ms
   - do some research on spritesheets, is it faster?
     - abstract textures into class - TextureSource, AnimatedTextureSource, etc.
   - Can cache wall slices? i.e. abstract away Texture source, then cache
     - 6 materials x 128 cols x 128 possible sizes
       - Maybe a lazy list, so only generates as needed, then cached
       - check RAM vs render time tradeoff
 
 - Hold multiple weapons and change between them with key presses
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
 - Bring sim runner into engine
   - provide own implementation of extract and apply Input
 - Optimise render - a lot of calculations can be done once
 - Speed up travis. See 
   - http://rundef.com/fast-travis-ci-docker-build 
   - https://gist.github.com/hc2p/9e284cee3d585eefbc59454e44cc247a
   - http://atodorov.org/blog/2017/08/07/faster-travis-ci-tests-with-docker-cache/
   - https://giorgos.sealabs.net/docker-cache-on-travis-and-docker-112.html
 - Try reading original data file?
 - Compare to real implementation - http://fabiensanglard.net/Game_Engine_Black_Book_Release_Date/index.php
 