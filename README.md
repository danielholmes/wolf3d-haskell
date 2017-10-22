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
   
 - merge packages to avoid circular refs (item, hero, w3dsim)
   - complete simitem def - pass world as part of update
 - render and shoot gun
 - static sprite targets that can shoot and kill
 - SimItem infrastructure - Update each item to produce events, process those events which generate new events
 - physics engine (2d top down) - do some research on physics engines and functional esp.
   - wolfs is simple
     1. Move x and y, if okay then return
     2. move x only, if okay return
     3. move y only, if oka return
     4. return
 
 - optimise rendering
   - rendering solid colur instead of textures improves from ~30ms - ~5ms
   - do some research on spritesheets, is it faster?
     - abstract textures into class - TextureSource, AnimatedTextureSource, etc.
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


## Nice to haves

 - Structure cabal project in such a way that modules not exposed to main can still be exposed to test
 - Optimise render - a lot of calculations can be done once
 - Speed up travis. See 
   - http://rundef.com/fast-travis-ci-docker-build 
   - https://gist.github.com/hc2p/9e284cee3d585eefbc59454e44cc247a
   - http://atodorov.org/blog/2017/08/07/faster-travis-ci-tests-with-docker-cache/
   - https://giorgos.sealabs.net/docker-cache-on-travis-and-docker-112.html
 - Try reading original data file?
 - Compare to real implementation - http://fabiensanglard.net/Game_Engine_Black_Book_Release_Date/index.php
 