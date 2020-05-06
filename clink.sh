clang++ -c libtonystd.cpp -O3 -o libtonystd.o
clang++ libtonystd.o libgc.so $1 -o $2
