# mfc
Fortran compiler in Haskell

# build

1. build by ghc

	cd src

	ghc Main.hs -o mfc -outputdir build

2. build by cabal

	cabal build macaf.cabal


# run test

${PATH}/mfc -a examples/test.macaf
