# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC=gcc
CFLAGS=-Wall -Werror -ansi -pedantic 
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

BIN= life
OBJS=
ansifile.o
file.o
generation.o
life.o
thinkcallback.o
thinkdisplay.o
thinkedit.o
thinkfile.o
thinkinit.o



all: $(BIN)

$(BIN): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

clean:
	rm onedee.o onedee pedant.o pedant
