CC?=cc
CFLAGS=-g --std=c11 -Wall -Wextra -Wno-unused-parameter -Werror -Wpedantic

BIN= mandelbrot
OBJS= mandelbrot.o
LIBS= -L/usr/local/lib -lpng -lm
LDFLAGS= -pg

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)
