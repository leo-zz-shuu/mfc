# mfc
Fortran compiler in Haskell

--------------------------------------------------
build

cd src 
ghc Main.hs -o mfc -outputdir build

--------------------------------------------------
run test

./mfc -a ./examples/test.macaf
