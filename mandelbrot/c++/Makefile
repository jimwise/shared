# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CXX=c++
CFLAGS=-g -Wall

BIN= mandelbrot
OBJS= mandelbrot.o
LIBS= -lpng -lm

all: $(BIN)

$(BIN): $(OBJS)
	$(CXX) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)
