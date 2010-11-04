# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC=gcc
CFLAGS=-Wall -Werror -ansi -pedantic 
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

BIN= life
OBJS= ansifile.o display.o edit.o file.o generation.o life.o

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

clean:
	rm $(BIN) $(OBJS)
