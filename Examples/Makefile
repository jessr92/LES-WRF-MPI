FC = mpifort
FCFLAGS += -fopenmp -cpp -Wall -Wextra -Wconversion -pedantic -std=f95 -fall-intrinsics -O2 -march=native
PROGRAMS = helloMPI helloOpenMP helloMPIOpenMP haloExchangeExample
HELLO_MPI_OBJS = helloMPI.o mpi_helper.o
HELLO_OPEN_MP_OBJS = helloOpenMP.o
HELLO_MPI_OPEN_MP_OBJS = helloMPIOpenMP.o mpi_helper.o
HALO_EXCHANGE_EXAMPLE_OBJS = haloExchangeExample.o mpi_helper.o

all: $(PROGRAMS)

helloMPI: $(HELLO_MPI_OBJS)

helloMPI.o : helloMPI.f95 mpi_helper.mod

helloOpenMP: $(HELLO_OPEN_MP_OBJS)

helloMPIOpenMP: $(HELLO_MPI_OPEN_MP_OBJS)

helloMPIOpenMP.o: helloMPIOpenMP.f95 mpi_helper.mod

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
