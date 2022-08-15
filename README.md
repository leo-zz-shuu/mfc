# mfc
Fortran compiler in Haskell

--------------------------------------------------
build

1. 
cd src. 
ghc Main.hs -o mfc -outputdir build. 

2.  
cabal build macaf.cabal
--------------------------------------------------
run test

./mfc -a ./examples/test.macaf
