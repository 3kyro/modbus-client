# Modbus Client

## Building Modbus Client

### Prerequisites

To build *Modbus Client* on Linux and Windows you will need the following tools installed:

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) - version >=2.5.1
- [Elm](https://guide.elm-lang.org/install/elm.html) - version 0.19.1
- [Rust](https://www.rust-lang.org/tools/install) - version >= 1.47.0 . Only for building the Modbus test server
- [NSIS](https://nsis.sourceforge.io/Download). Only for building *Modbus Client* on Windows. You will need to add the NSIS folder to your path after installation 

### Building using `build.hs`

You can use the provided `build.hs` script to build *Modbus Client* backend and frontend along with a Modbus test server, as well as Linux and Windows installers. The script can only be run inside the application's root folder.

Linux: 
````
> ./build.hs
````

Windows:
````
> stack runghc .\build.hs
````
