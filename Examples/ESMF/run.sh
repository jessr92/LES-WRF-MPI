make
mpiexec -f ~/LES-WRF-MPI/machinefilemaconly -np 4 ~/LES-WRF-MPI/Examples/ESMF/esmfExampleCoupling
cat PET* | sort -k 2,2 > esmfExampleCouplingLog
