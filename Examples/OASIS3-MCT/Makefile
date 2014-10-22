include /home/mpi-user/LES-WRF-MPI/oasis3-mct/util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a

F90FLAGS += -W -Wall -O2 -fopenmp
PROGRAMS = oasisExampleA oasisExampleB
OASIS_EXAMPLE_A_OBJS = oasisHelpers.o oasisExampleA.o
OASIS_EXAMPLE_B_OBJS = oasisHelpers.o oasisExampleB.o

all: $(PROGRAMS)

oasisExampleA: $(OASIS_EXAMPLE_A_OBJS) $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@  $(OASIS_EXAMPLE_A_OBJS) $(LIBPSMILE) $(FLIBS)

oasisExampleB: $(OASIS_EXAMPLE_B_OBJS) $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@  $(OASIS_EXAMPLE_B_OBJS) $(LIBPSMILE) $(FLIBS)

%: %.o
	$(F90) -o $@ $^

%.mod: %.f95
	$(F90) $(F90FLAGS) -c -o $@ $<

%.o: %.f95
	$(F90) $(F90FLAGS) -c -o $@ $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}
