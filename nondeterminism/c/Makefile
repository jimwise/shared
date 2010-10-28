BIN=	choose2 parlortrick findboxes
INCS=	nondeterminism.h

CC=gcc
CFLAGS=	-g
LIBS = nondeterminism.o

all: $(BIN)

choose2: choose2.o nondeterminism.o
	$(CC) $(CFLAGS) -o $@ choose2.o nondeterminism.o

parlortrick: parlortrick.o nondeterminism.o
	$(CC) $(CFLAGS) -o $@ parlortrick.o nondeterminism.o

findboxes: findboxes.o nondeterminism.o
	$(CC) $(CFLAGS) -o $@ findboxes.o nondeterminism.o

clean:
	rm -f $(BIN) *.o
