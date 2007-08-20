@echo off
pushd test\%1
mkdir obj 2> nul
mkdir obj\supero 2> nul
mkdir obj\haskell 2> nul

ghc --make -O2 4.hs -o supero.exe -hidir obj\supero -odir obj\supero
ghc --make -O2 %1.hs -o haskell.exe -hidir obj\haskell -odir obj\haskell

call arguments.bat

echo Benchmarking Supero
timer supero %args%
echo.

echo Benchmarking Haskell
timer haskell %args%
echo.

echo Benchmarking Supero
timer supero %args%
echo.

echo Benchmarking Haskell
timer haskell %args%
echo.

echo Benchmarking Supero
timer supero %args%
echo.

popd

