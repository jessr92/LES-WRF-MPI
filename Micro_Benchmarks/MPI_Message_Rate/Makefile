FC = mpifort
FCFLAGS += -std=f95

all: message_rate

message_rate: message_rate.o

message_rate.o: message_rate.f95

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

