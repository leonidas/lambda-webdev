
# Haskell Web Game Example

This is an example implementation of a browser based Tic-Tac-Toe using [CoffeeScript](http://coffeescript.org) and [Haskell](http://www.haskell.org/haskellwiki/Haskell).
The client-side logic is implemented using [Knockout.js](http://knockoutjs.com)
and WebSockets are used for two-way communication between the client and server.

## How to build the project

First, install the [Haskell Platform](http://www.haskell.org/platform/) and get
the latest repository information by running

    cabal update

Next, you'll need [Node.js](http://nodejs.org/) for running the
Jade/CoffeeScript/Stylus tool-chain. Best way to install Node is by using [NVM](https://github.com/creationix/nvm).

### Clone the repository

    git clone git://github.com/leonidas/lambda-webdev.git
    cd lambda-webdev
    git checkout type-safe

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

This will launch a web server that listens to the port 8000.
