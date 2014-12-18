#!/bin/sh
# Setup timing runs
HOSTNAME=$(hostname)
TIMING_DIRECTORY="timingRuns/"$HOSTNAME
MAX_PER_DIMENSION=16
mkdir -p $TIMING_DIRECTORY
mkdir -p $TIMING_DIRECTORY"/MPI_SharedMemory"
HARDWARE_THREAD_COUNT=$(grep -c ^processor /proc/cpuinfo)
# Max per dimension needs to be at least enough such that NxN will use all
# available hardware threads on the shared memory system
if [ `expr $MAX_PER_DIMENSION \\* $MAX_PER_DIMENSION` -lt $HARDWARE_THREAD_COUNT ]; then
    MAX_PER_DIMENSION=$(echo "sqrt($HARDWARE_THREAD_COUNT)" | bc -l)
    MAX_PER_DIMENSION=$(echo $(( `echo $MAX_PER_DIMENSION|cut -f1 -d"."` + 1 )))
fi
MAX_DIMENSION_PROCESSES=$HARDWARE_THREAD_COUNT
if [ $MAX_DIMENSION_PROCESSES -gt $MAX_PER_DIMENSION ]; then
    MAX_DIMENSION_PROCESSES=$MAX_PER_DIMENSION
fi
# Execute original code
scons ocl=0 mpi=0 D=TIMINGS
./les_main > $TIMING_DIRECTORY"/les_main.txt"
# Execute MPI code for all combinations that fit on the number of hardware threads
for procPerRow in $(seq $MAX_DIMENSION_PROCESSES)
do
    for procPerCol in $(seq $MAX_DIMENSION_PROCESSES)
    do
        PROCESSES=`expr $procPerCol \\* $procPerRow`
        if [ $PROCESSES -le $HARDWARE_THREAD_COUNT ]; then
            OUTPUT_FILE=$TIMING_DIRECTORY"/MPI_SharedMemory/les_main_mpi_row"$procPerRow"_col"$procPerCol".txt"
            scons ocl=0 mpi=1 D=TIMINGS procPerRow=$procPerRow procPerCol=$procPerCol
            mpiexec -np $PROCESSES ./les_main_mpi > $OUTPUT_FILE
        fi
    done
done

