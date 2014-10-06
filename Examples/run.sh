#!/bin/sh
make
mpiexec -f ~/LES-WRF-MPI/machinefile -n 8 ~/LES-WRF-MPI/Examples/helloMPI
~/LES-WRF-MPI/Examples/helloOpenMP

