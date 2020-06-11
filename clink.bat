
call clang++ -O3 -c libtonystd.cpp -o libtonystd.obj
call clang -O3 libtonystd.obj %1 -o %2 libgcmt.lib