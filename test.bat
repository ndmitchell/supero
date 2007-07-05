@echo off
pushd test\%1
mkdir obj 2> nul
mkdir obj\supero 2> nul
mkdir obj\super 2> nul

ghc --make -O2 4.hs -o supero.exe -hidir obj\supero -odir obj\supero
type 4.hs | supero.exe

ghc --make -O0 4.hs -o super.exe -hidir obj\super -odir obj\super
type 4.hs | super.exe

popd
