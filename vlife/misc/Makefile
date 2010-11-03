# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC=gcc
CFLAGS=-Wall -Werror -ansi -pedantic 
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

all: onedee pedant
onedee: onedee.o
	$(CC) -o onedee onedee.o -lcurses

pedant: pedant.o
	$(CC) -o pedant pedant.o -lcurses

clean:
	rm onedee.o onedee pedant.o pedant
