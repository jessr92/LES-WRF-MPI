#!/bin/sh

scons ocl=0 mpi=0 verbose=1 gr_debug=1 vw_debug=1
mv les_main les_main_debug

scons ocl=0 mpi=1 verbose=1 gr_debug=1 vw_debug=1
mv les_main_mpi les_main_mpi_debug

scons ocl=0 mpi=0 verbose=1
scons ocl=0 mpi=1 verbose=1


