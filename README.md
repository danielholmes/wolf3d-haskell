# Wolfenstein 3D in Haskell

[![Build Status](https://travis-ci.org/danielholmes/wolf3d-haskell.svg?branch=master)](https://travis-ci.org/danielholmes/wolf3d-haskell)

Functional Wolfenstein 3D engine implementation in Haskell.


## Requirements

 - [Stack](https://www.haskellstack.org) (Tested with 1.4)
 - [SDL2 2.0.4+](https://www.libsdl.org/)
 - [SDL2 Image](https://www.libsdl.org/projects/SDL_image/)


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

 - SimItem infrastructure - Update each item to produce events, process those events which generate new events
 - physics engine (2d top down)
 - Dont render hidden sprites
   - only render within field of view bounds
   - only render in front of walls
 - Example world that shows off diagonal walls, etc
 - sprite items block movement
 - render and shoot gun
 - static sprite targets that can shoot and kill
 - pickup items
 - fix travis build
 - All events together at the end of game loop passed to audio handler, which plays audio
 - mouse look horizontal
 - mouse look up/down (move projection plane up/down)
 - SDL dispose textures at the end


## Nice to haves

 - Num,Fractional,Rational instances where appropriate for PosZDouble, etc
 - FPS/ms/render time, etc stats in debug view
 - Optimise render - a lot of calculations can be done once
 - Find better PosInt and PosZInt implementation
 - Try reading original data file?
 - Compare to real implementation - http://fabiensanglard.net/Game_Engine_Black_Book_Release_Date/index.php
 