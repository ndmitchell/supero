@echo off
pushd test\%1

del 4.o
del 4.hi
del 4.exe
ghc --make -O2 4.hs
type 4.hs | 4.exe

del 4.o
del 4.hi
del 4.exe
ghc --make -O0 4.hs
type 4.hs | 4.exe

popd
