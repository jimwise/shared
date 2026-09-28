CC?=cc
CFLAGS=-O3 --std=c23 -Wall -Wextra -Wno-unused-parameter -Werror -Wpedantic -I/opt/homebrew/include -I/opt/homebrew/opt/libomp/include -Xpreprocessor -fopenmp
OPENMP_FLAGS=-lomp

BIN= mandelbrot
OBJS= mandelbrot.o
LIBS= -L/opt/homebrew/lib -L/opt/homebrew/opt/libomp/lib -lpng -lm $(OPENMP_FLAGS)
LDFLAGS= -pg

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)
