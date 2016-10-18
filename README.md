# raycast
Haskell-based raycasting engine

##raycast/
raycast/ contains a Stack project to build the Haskell executable for the project


## RaycastRuntime/
RaycastRuntime/ contains a MSVC project to build a helper C DLL that contains some rendering/window management.

## Building
The project must be built in 2 parts:

1. **Runtime DLL**

 Build the RaycastRuntime project. It should automatically produce RaycastRuntime.dll, RaycastRuntime.Def, libRaycastRuntime.a and HaskellStructs.h
 in the raycast/ folder, which are all needed by ghc to link the Haskell project.

2. **raycast.exe**

 Build raycast from the raycast/ folder with `stack install`. (You could also `stack build`, but if you want to debug from Visual Studio,
 it needs a path to the exe to run, so I've set it up to debug from the installed exe that gets placed in `%APPDATA%`)
 
The project should be runnable now with any of `stack exec raycast level_file`, `raycast level_file` (if built with `stack install`)
or by running from Visual Studio (must have used `stack install`, and will automatically run with `level.txt`)
