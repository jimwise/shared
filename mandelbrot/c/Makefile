# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC=gcc
CFLAGS=-g -Wall -Werror -ansi -pedantic -std=c99
CPPFLAGS= -D__EXTENSIONS__ -I/opt/csw/include

BIN= mandelbrot
OBJS= mandelbrot.o
LIBS=-L/opt/csw/lib -R/opt/csw/lib -lpng -lm

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)
