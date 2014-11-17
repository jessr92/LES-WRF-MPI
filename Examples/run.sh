#!/bin/sh
set -e
echo '**********'
echo 'Compiling examples'
echo '**********'
make
echo '**********'
echo 'Running Single Threaded MPI Hello World'
echo '**********'
mpiexec -n 2 ~/LES-WRF-MPI/Examples/helloMPI
echo '**********'
echo 'Running Single Node OpenMP Hello World'
echo '**********'
~/LES-WRF-MPI/Examples/helloOpenMP
echo '**********'
echo 'Running MPI OpenMP Hello World'
echo '**********'
mpiexec -n 4 ~/LES-WRF-MPI/Examples/helloMPIOpenMP
echo '**********'
echo 'Running MPI Halo Exchange Example'
echo '*********'
mpiexec -n 4 ~/LES-WRF-MPI/Examples/haloExchangeExample
echo '*********'
