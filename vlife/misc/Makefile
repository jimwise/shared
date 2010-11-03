# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC=gcc
CFLAGS=-Wall -Werror -ansi -pedantic 
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

pedant: pedant.o
	$(CC) -o pedant pedant.o -lcurses

clean:
	rm pedant.o pedant
