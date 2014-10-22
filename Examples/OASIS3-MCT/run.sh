#/bin/sh
make clean
make
mpiexec -n 1 ~/LES-WRF-MPI/Examples/OASIS3-MCT/oasisExampleA : -n 1 ~/LES-WRF-MPI/Examples/OASIS3-MCT/oasisExampleB
