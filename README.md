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


## TODO

 - 2d ray casted static scene (2d ray casting then choose column of image based on what hits)
   - 2d solid colour (roof and floor)
   - 2d with image texture - http://3d.wolfenstein.com/game/wolf3d.html
 - fix travis build
 - constantly rotating ray casted scene
 - keyboard movement within world
   - Update each item to produce events, process those events which generate new events
 - provide top down rendering also for debugging (maybe separate exe?)
 - physics engine (2d top down)
 - sprite items that block movement
 - render and shoot gun
 - static sprite targets that can shoot and kill
 - pickup items
 - All events together at the end of game loop passed to audio handler, which plays audio


## Nice to haves

 - Find better PosInt and PosZInt implementation
 - Try reading original data file?
 