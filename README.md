# htrace - a raytracer in Haskell

This is a little raytracer that i write/wrote for the lecture computergraphics in the winter 14/15.

## Compiling

```bash
# clone the repository
git clone https://github.com/Drezil/htrace
# init a cabal sandbox for the dependencies
cabal sandbox init
# download & build dependencies
cabal install --only-dependencies
# build the program
cabal build
# run
./raytrace.sh scenes/spheres/spheres.sce
```

If the building fails with some llvm-related errer you don't seem to have LLVM installed. Either install llvm (libghc-llvm-base-dev for ubuntu) or remove the ```-fllvm```-flag from the ghc-options in the raytrace.cabal.
