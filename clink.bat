
call "Z:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat" x64
cl /MT /c libtonystd.cpp
link libgcmt.lib libtonystd.obj %1 /out:%2