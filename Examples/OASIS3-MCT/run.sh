#/bin/sh
make
mpirun -np 2 ~/LES-WRF-MPI/Examples/OASIS3-MCT/oasisExampleA : -np 2 ~/LES-WRF-MPI/Examples/OASIS3-MCT/oasisExampleB
