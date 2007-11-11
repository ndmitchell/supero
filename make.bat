set libs=d:\sources\yhc\current\src\libraries\core
if not exist %libs% set libs=C:\Documents\Uni\yhc\current\src\libraries\core

if "%1"=="" ghc --make Main.hs -i%libs% -fglasgow-exts -hidir obj -odir obj -o supero.exe
if "%1"=="i" ghci Main.hs -i%libs% -fglasgow-exts -hidir obj -odir obj

