FC = mpifort
FCFLAGS += -fopenmp -cpp -Wall -Wextra -Wconversion -pedantic -std=f95 -fall-intrinsics -O2
PROGRAMS = helloMPI helloOpenMP helloMPIOpenMP haloExchangeExample haloExchange3DExample mpi_pi
HELLO_MPI_OBJS = helloMPI.o mpi_helper.o
HELLO_OPEN_MP_OBJS = helloOpenMP.o
HELLO_MPI_OPEN_MP_OBJS = helloMPIOpenMP.o mpi_helper.o
HALO_EXCHANGE_EXAMPLE_OBJS = haloExchangeExample.o mpi_helper.o
HALO_EXCHANGE_3D_EXAMPLE_OBJS = haloExchange3DExample.o mpi_helper.o
MPI_PI_OBJS = mpi_pi.o mpi_helper.o

all: $(PROGRAMS)

helloMPI: $(HELLO_MPI_OBJS)

helloMPI.o : helloMPI.f95 mpi_helper.mod

helloOpenMP: $(HELLO_OPEN_MP_OBJS)

helloMPIOpenMP: $(HELLO_MPI_OPEN_MP_OBJS)

helloMPIOpenMP.o: helloMPIOpenMP.f95 mpi_helper.mod

haloExchangeExample: $(HALO_EXCHANGE_EXAMPLE_OBJS)

haloExchangeExample.o: haloExchangeExample.f95 mpi_helper.mod

haloExchange3DExample: $(HALO_EXCHANGE_3D_EXAMPLE_OBJS)

haloExchange3DExample.o: haloExchange3DExample.f95 mpi_helper.mod

mpi_pi: $(MPI_PI_OBJS)

mpi_pi.o: mpi_pi.f95 mpi_helper.mod

mpi_helper.mod: ../LES/F95Sources/mpi_helper.f95
	$(FC) $(FCFLAGS) -c $<

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}
