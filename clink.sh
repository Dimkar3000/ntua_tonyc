clang++ -c libtonystd.cpp -O3 -o libtonystd.o
clang++ libtonystd.o $1 -o $2 -lgc
