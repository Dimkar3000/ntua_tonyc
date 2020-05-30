
call clang++ -c libtonystd.cpp -o libtonystd.obj
call clang libtonystd.obj %1 -o %2