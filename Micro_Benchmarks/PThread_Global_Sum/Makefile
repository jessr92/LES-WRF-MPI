CFLAGS=-std=c99 -pthread

all: global_sum

global_sum: global_sum.o

global_sum.o: global_sum.c

%: %.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$.o: $.c
	$(CC) $(CFLAGS) -c %<

