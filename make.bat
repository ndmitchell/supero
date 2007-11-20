@echo off
if not exist obj mkdir obj
if not exist obj\prof mkdir obj\prof

set libs=d:\sources\yhc\current\src\libraries\core
if not exist %libs% set libs=C:\Documents\Uni\yhc\current\src\libraries\core
if not exist %libs% set libs=C:\Neil\yhc\src\libraries\core

if "%1"=="" ghc --make Main.hs -i%libs% -hidir obj -odir obj -o supero.exe
if "%1"=="i" ghci Main.hs -i%libs% -hidir obj -odir obj
if "%1"=="p" ghc -prof -auto-all --make Main.hs -i%libs% -hidir obj\prof -odir obj\prof -o superop.exe
