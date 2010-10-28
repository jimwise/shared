BIN=	choose2 parlortrick findboxes
INCS=	nondeterminism.h

CC=gcc
CFLAGS=	-g

all: $(BIN)

%: %.o nondeterminism.o
	$(CC) $(CFLAGS) -o $@ $< nondeterminism.o

clean:
	rm -f $(BIN) *.o
