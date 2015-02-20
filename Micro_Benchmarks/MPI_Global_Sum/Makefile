FC = mpifort
FCFLAGS += -std=f95

all: global_sum

global_sum: global_sum.o

global_sum.o: global_sum.f95

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

