for /l %%i in (1,1,10) do yhc -linkcore Example%%i.hs
yhc -core Primitive.hs
mkdir ..\test > nul
copy *.yca ..\test
copy Primitive.ycr ..
