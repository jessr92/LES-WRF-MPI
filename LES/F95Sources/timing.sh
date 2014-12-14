#!/bin/sh
# Setup timing runs
HOSTNAME=$(hostname)
TIMING_DIRECTORY="timingRuns/"$HOSTNAME
mkdir -p $TIMING_DIRECTORY
HARDWARE_THREAD_COUNT=$(grep -c ^processor /proc/cpuinfo)
# Execute original code
scons ocl=0 mpi=0 D=TIMINGS
./les_main 2>&1 | tee $TIMING_DIRECTORY"/les_main.txt"
# Execute MPI code for all combinations that fit on the number of hardware threads
for procPerRow in $(seq $HARDWARE_THREAD_COUNT)
do
    for procPerCol in $(seq $HARDWARE_THREAD_COUNT)
    do
        PROCESSES=`expr $procPerCol \\* $procPerRow`
        if [ $PROCESSES -le $HARDWARE_THREAD_COUNT ]; then
            OUTPUT_FILE=$TIMING_DIRECTORY"/les_main_mpi_row"$procPerRow"_col"$procPerCol".txt"
            scons ocl=0 mpi=1 D=TIMINGS procPerRow=$procPerRow procPerCol=$procPerCol
            mpiexec -np $PROCESSES ./les_main_mpi 2>&1 | tee $OUTPUT_FILE
        fi
    done
done

