FC = mpifort
FCFLAGS += -W -Wall -O2 -fopenmp -Wno-unused-parameter
PROGRAMS = esmfExampleCoupling
ESMF_EXAMPLE_COUPLING_OBJS = esmfExampleCoupling.o componentOne.o componentTwo.o couplerComponent.o esmfHelpers.o

ESMF_LIB=$(ESMF_DIR)/lib/libO/Linux.gfortran.64.mpiuni.default
ESMF_MOD=$(ESMF_DIR)/mod/modO/Linux.gfortran.64.mpiuni.default

include $(ESMF_LIB)/esmf.mk

FCFLAGS+=-I$(ESMF_MOD)

all: $(PROGRAMS)

esmfExampleCoupling: $(ESMF_EXAMPLE_COUPLING_OBJS)

esmfExampleCoupling.o: esmfHelpers.mod componentOne.mod componentTwo.mod couplerComponent.mod esmfExampleCoupling.f95

%: %.o
	$(FC) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) \
	$(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c -o $@ $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c -o $@ $<

clean:
	rm *.o *.mod *~ ${PROGRAMS}

