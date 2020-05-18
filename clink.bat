
call "Z:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat" x64
call cl /MT /EHsc /c libtonystd.cpp
call link libtonystd.obj %1 /out:%2