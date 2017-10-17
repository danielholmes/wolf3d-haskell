# Wolfenstein 3D in Haskell

[![Build Status](https://travis-ci.org/danielholmes/wolf3d-haskell.svg?branch=master)](https://travis-ci.org/danielholmes/wolf3d-haskell)

Functional Wolfenstein 3D engine implementation in Haskell.


## Requirements

 - [Stack](https://www.haskellstack.org) (Tested with 1.4)
 - [SDL2](https://www.libsdl.org/)
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
