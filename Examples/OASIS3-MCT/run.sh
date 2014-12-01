#/bin/sh
make clean
make
mpiexec -n 1 $(pwd)/oasisExampleA : -n 1 $(pwd)/oasisExampleB
