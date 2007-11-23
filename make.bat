@echo off
if not exist obj mkdir obj
if not exist obj\prof mkdir obj\prof
if not exist obj\opt mkdir obj\opt
if not exist obj\norm mkdir obj\norm

set libs=d:\sources\yhc\current\src\libraries\core
if not exist %libs% set libs=C:\Documents\Uni\yhc\current\src\libraries\core
if not exist %libs% set libs=C:\Neil\yhc\src\libraries\core

if "%1"=="" ghc --make Main.hs -i%libs% -hidir obj\norm -odir obj\norm -o supero.exe
if "%1"=="i" ghci Main.hs -i%libs% -hidir obj\norm -odir obj\norm
if "%1"=="p" ghc -prof -auto-all --make Main.hs -i%libs% -hidir obj\prof -odir obj\prof -o superop.exe
if "%1"=="o" ghc -O2 --make Main.hs -i%libs% -hidir obj\opt -odir obj\opt -o supero.exe
