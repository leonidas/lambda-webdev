# Haskell Rich Web Application Example

This exercise will demonstrate how to write a Haskell back-end for a dynamic,
real-time web application. We will write the client-side logic using
[CoffeeScript](http://coffeescript.org) and [Knockout.js](http://knockoutjs.com)
and implement two-way communication with WebSockets on top of a Haskell web
server.

## How to get started

First, install the [Haskell Platform](http://www.haskell.org/platform/) and get
the latest repository information by running

    cabal update

Next, you'll need [Node.js](http://nodejs.org/) for running the
Jade/CoffeeScript/Stylus tool-chain. Best way to install Node is by using [NVM](https://github.com/creationix/nvm).

### Clone the repository

    git clone git://github.com/leonidas/lambda-webdev.git
    cd lambda-webdev

### Get the required JavaScript tools

    npm install

### Using cabal-dev

`cabal-dev` is a tool for installing packages in an isolated project
environment instead of installing libraries globally. It makes it easier to
avoid dependency conflicts but you have to rebuild everything you install so it
takes some time to prepare a new project.

    cabal install cabal-dev
    cabal-dev configure
    cabal-dev install-deps
    cabal-dev build

You also need to modify the file `grunt.coffee` and set

    useCabalDev   = true

at the top of the file.

### Using ordinary cabal

There are some situations where `cabal-dev` doesn't work correctly on MacOS X
so you might opt to use the regular cabal instead.

    cabal configure
    cabal install --only-dependencies
    cabal build

### Starting the server

    ./grunt


## Editing Haskell sources

In order to get proper auto-complete and other goodies, you should install [`ghc-mod`](http://www.mew.org/~kazu/proj/ghc-mod/en/).

    cabal install ghc-mod

The tool works with either vim, emacs or Sublime Text (via [SublimeHaskell](https://github.com/SublimeHaskell/SublimeHaskell)). For SublimeHaskell, you also need to run

    cabal install aeson haskell-src-exts
