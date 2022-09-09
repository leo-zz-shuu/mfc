# mfc

Fortran compiler in Haskell

## build

1. build by ghc

```sh
   cd src
   ghc Main.hs -o mfc -outputdir build
```

2. build by cabal

```sh
   cabal build
```

## run test

```sh
\${PATH}/mfc -a examples/test.macaf
```

## coding style

We use `stylish-haskell` for code formatting. Please install and configure it following its instructions [here](https://github.com/haskell/stylish-haskell#installation).

Make sure you have `stylish-haskell` installed and run `make format` before committing.
