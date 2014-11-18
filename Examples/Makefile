FC = mpifort
FCFLAGS = -fopenmp -W -Wall -Wno-unused-parameter -g -Og -fbounds-check -fbacktrace -finit-real=nan -fcheck=all
PROGRAMS = helloMPI helloOpenMP helloMPIOpenMP haloExchangeExample
HELLO_MPI_OBJS = helloMPI.o
HELLO_OPEN_MP_OBJS = helloOpenMP.o
HELLO_MPI_OPEN_MP_OBJS = helloMPIOpenMP.o
HALO_EXCHANGE_EXAMPLE_OBJS = haloExchangeExample.o mpi_helper.o

all: $(PROGRAMS)

helloMPI: $(HELLO_MPI_OBJS)

helloOpenMP: $(HELLO_OPEN_MP_OBJS)

helloMPIOpenMP: $(HELLO_MPI_OPEN_MP_OBJS)

haloExchangeExample: $(HALO_EXCHANGE_EXAMPLE_OBJS)

haloExchangeExample.o : haloExchangeExample.f95 mpi_helper.mod

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}
