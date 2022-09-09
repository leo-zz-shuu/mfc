# mfc

Fortran compiler in Haskell

## build

1. build by ghc

   cd src

   ghc Main.hs -o mfc -outputdir build

2. build by cabal

   cabal build macaf.cabal

## run test

\${PATH}/mfc -a examples/test.macaf

## code style

We use `stylish-haskell` for code formatting. Please install and configure it following its instructions [here](https://github.com/haskell/stylish-haskell#installation).
