CXX?=c++
CFLAGS=-g --std=c++11 -Wall -Wextra -Wpedantic -Werror

BIN= mandelbrot
OBJS= mandelbrot.o
LIBS= -lpng -lm

all: $(BIN)

$(BIN): $(OBJS)
	$(CXX) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)
