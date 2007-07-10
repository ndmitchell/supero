@echo off

set data=d:\data2.txt

pushd test\%1

echo Benchmarking %2
type %data% | timer %2
type %data% | timer %2

REM Don't do Haskell as much, because its very slow!
if %2==haskell goto end
type %data% | timer %2
type %data% | timer %2
type %data% | timer %2
echo.


:end

popd
