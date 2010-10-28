BIN=	example
SRCS=	example.c nondeterminism.c
INCS=	nondeterminism.h

CC=gcc
CFLAGS=	-g

all: $(BIN)

$(BIN): $(SRCS) $(INCS)
	$(CC) $(CFLAGS) -o $(BIN) $(SRCS) 

clean:
	rm -f $(BIN)
