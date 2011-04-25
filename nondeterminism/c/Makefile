BIN=	choose2 parlortrick findboxes
INCS=	nondeterminism.h

CC=gcc
CFLAGS=	-g -Wall
LIBS = nondeterminism.o

all: $(BIN)

choose2: choose2.o $(LIBS)
	$(CC) $(CFLAGS) -o $@ $^

parlortrick: parlortrick.o $(LIBS)
	$(CC) $(CFLAGS) -o $@ $^

findboxes: findboxes.o $(LIBS)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(BIN) *.o
