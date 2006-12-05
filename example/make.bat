for /l %%i in (1,1,6) do yhc -linkcore Example%%i.hs
copy *.yca ..
copy Primitive.ycr ..
