CFLAGS=-std=c11 -W -Wall -Wextra -pedantic
CFLAGS+=-pthread -O2

all: global_sum

global_sum: global_sum.o

global_sum.o: global_sum.c

%: %.o
	$(CXX) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$.o: $.c
	$(CXX) $(CFLAGS) -c %<

clean:
	rm *.o
