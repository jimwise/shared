CC?=gcc
CFLAGS=-g -Wall -Werror -ansi -pedantic 
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

BIN= life
OBJS= board.o display.o edit.o file.o life.o

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) $(OBJS)

$(OBJS): life.h
