CC=cc
CFLAGS=-g -std=c11 -Wall -Wextra -Wno-unused-parameter -Werror -Wpedantic 

BIN= blackjack
OBJS= main.o 
LIBOBJS= io.o blackjack.o cards.o
LIBS=-lm
INCS= cards.h

all: $(BIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

test: test.o $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) test test.o $(OBJS) $(LIBOBJS)

$(OBJS): $(INCS)
