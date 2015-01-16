FC = mpifort
FCFLAGS += -fopenmp -cpp -Wall -Wextra -Wconversion -pedantic -std=f95 -fall-intrinsics -O2
PROGRAMS = helloMPI helloOpenMP helloMPIOpenMP haloExchange3DExample haloExchange3DRealExample sideflowExample cartesianVirtualTopologyExample mpi_pi
HELPER_OBJS = communication_helper.o communication_helper_integer.o communication_helper_base.o communication_helper_real.o fortran_helper.o
HELPER_MODS = communication_helper_base.mod communication_helper_integer.mod communication_helper_real.mod communication_helper.mod fortran_helper.mod
HELLO_MPI_OBJS = helloMPI.o $(HELPER_OBJS)
HELLO_OPEN_MP_OBJS = helloOpenMP.o
HELLO_MPI_OPEN_MP_OBJS = helloMPIOpenMP.o $(HELPER_OBJS)
HALO_EXCHANGE_3D_EXAMPLE_OBJS = haloExchange3DExample.o $(HELPER_OBJS)
HALO_EXCHANGE_3D_REAL_EXAMPLE_OBJS = haloExchange3DRealExample.o $(HELPER_OBJS)
SIDEFLOW_EXAMPLE_OBJS = sideflowExample.o $(HELPER_OBJS)
CARTESIAN_VIRTUAL_TOPOLOGY_EXAMPLE_OBJS = cartesianVirtualTopologyExample.o $(HELPER_OBJS)
MPI_PI_OBJS = mpi_pi.o $(HELPER_OBJS)

all: $(PROGRAMS)

helloMPI: $(HELLO_MPI_OBJS)

helloMPI.o : helloMPI.f95 $(HELPER_MODS)

helloOpenMP: $(HELLO_OPEN_MP_OBJS)

helloMPIOpenMP: $(HELLO_MPI_OPEN_MP_OBJS)

helloMPIOpenMP.o: helloMPIOpenMP.f95 $(HELPER_MODS)

haloExchange3DExample: $(HALO_EXCHANGE_3D_EXAMPLE_OBJS)

haloExchange3DExample.o: haloExchange3DExample.f95 $(HELPER_MODS)

haloExchange3DRealExample: $(HALO_EXCHANGE_3D_REAL_EXAMPLE_OBJS)

haloExchange3DRealExample.o: haloExchange3DRealExample.f95 $(HELPER_MODS)

sideflowExample: $(SIDEFLOW_EXAMPLE_OBJS)

sideflowExample.o: sideflowExample.f95 $(HELPER_MODS)

cartesianVirtualTopologyExample: $(CARTESIAN_VIRTUAL_TOPOLOGY_EXAMPLE_OBJS)

cartesianVirtualTopologyExample.o: cartesianVirtualTopologyExample.f95 $(HELPER_MODS)

mpi_pi: $(MPI_PI_OBJS)

mpi_pi.o: mpi_pi.f95 $(HELPER_MODS)

communication_helper.mod: ../LES/F95Sources/communication_helper.f95
	$(FC) $(FCFLAGS) -c $<

communication_helper_base.mod: ../LES/F95Sources/communication_helper_base.f95
	$(FC) $(FCFLAGS) -c $<

communication_helper_integer.mod: ../LES/F95Sources/communication_helper_integer.f95
	$(FC) $(FCFLAGS) -c $<

communication_helper_real.mod: ../LES/F95Sources/communication_helper_real.f95
	$(FC) $(FCFLAGS) -c $<

fortran_helper.mod: ../LES/F95Sources/fortran_helper.f95
	$(FC) $(FCFLAGS) -c $<

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}
