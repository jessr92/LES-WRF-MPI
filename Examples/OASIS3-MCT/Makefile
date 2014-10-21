include /home/mpi-user/LES-WRF-MPI/oasis3-mct/util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a

F90FLAGS += -W -Wall -O2 -fopenmp
PROGRAMS = oasisExample
OASIS_EXAMPLE_OBJS = oasisExample.o

FCFLAGS += -lnetcdf -lnetcdff $(LIBPSMILE)

all: $(PROGRAMS)

%: %.o
	$(F90) -o $@ $^

%.mod: %.f95
	$(F90) $(F90FLAGS) -c -o $@ $<

%.o: %.f95
	$(F90) $(F90FLAGS) -c -o $@ $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}
