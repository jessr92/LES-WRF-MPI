FC = mpifort
FCFLAGS = -W -Wall -Wno-unused-parameter -O2
PROGRAMS = hello
HELLO_OBJS = hello.o

all: $(PROGRAMS)

hello: $(HELLO_OBJS)

hello.o : hello.f95

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.mod: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

clean:
	rm *.o ${PROGRAMS}
