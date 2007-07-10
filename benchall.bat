@echo off

call bench2 charcount haskell
call bench2 charcount c
call bench2 charcount supero

call bench2 linecount haskell
call bench2 linecount c
call bench2 linecount supero

call bench2 wordcount haskell
call bench2 wordcount c
call bench2 wordcount supero

:end

popd
