#!/bin/sh
make clean
set -e
echo '**********'
echo 'Compiling examples'
echo '**********'
make
echo '**********'
echo 'Running Single Threaded MPI Hello World'
echo '**********'
mpiexec -n 2 $(pwd)/helloMPI
echo '**********'
echo 'Running Single Node OpenMP Hello World'
echo '**********'
$(pwd)/helloOpenMP
echo '**********'
echo 'Running MPI OpenMP Hello World'
echo '**********'
mpiexec -n 4 $(pwd)/helloMPIOpenMP
echo '**********'
echo 'Running MPI PI Example'
echo '**********'
mpiexec -n 4 $(pwd)/mpi_pi
echo '**********'
echo 'Running MPI Halo Exchange 3D Example'
echo '*********'
mpiexec -n 9 $(pwd)/haloExchange3DExample
echo '*********'

