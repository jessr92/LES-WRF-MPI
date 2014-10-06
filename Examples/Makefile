FC = mpifort
FCFLAGS = -fopenmp -W -Wall -Wno-unused-parameter -O2
PROGRAMS = helloMPI helloOpenMP
HELLO_MPI_OBJS = helloMPI.o
HELLO_OPEN_MP_OBJS = helloOpenMP.o

all: $(PROGRAMS)

helloMPI: $(HELLO_MPI_OBJS)

helloOpenMP: $(HELLO_OPEN_MP_OBJS)

helloMPI.o: helloMPI.f95

helloOpenMP.o: helloOpenMP.f95

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o *~ ${PROGRAMS}
