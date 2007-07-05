@echo off

set data=d:\data.txt

call test.bat %1
pushd test\%1
mkdir obj\c 2> nul
mkdir obj\haskell 2> nul

ghc -optc-O3 Example.c -odir obj\c -o c.exe

ghc --make -O2 ExampleSingle.hs -o haskell.exe -hidir obj\haskell -odir obj\haskell

echo Benchmarking C
type %data% | timer c
echo.

echo Benchmarking Haskell
type %data% | timer haskell
echo.

echo Benchmarking Supero
type %data% | timer supero
echo.


popd

