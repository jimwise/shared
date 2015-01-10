# CC=cc
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes
CC?=gcc
CFLAGS=-Wall -Wextra -Wno-unused-parameter -Werror -ansi -pedantic 
CPPFLAGS= -D__EXTENSIONS__
LIBS= -lcurses

SUBDIRS= conway

all: onedee pedant subdirs

onedee: onedee.o
	$(CC) -o $@ $^ $(LIBS)

pedant: pedant.o
	$(CC) -o $@ $^ $(LIBS)

subdirs:
	for X in $(SUBDIRS); do make -C $$X ;done

clean: cleansubdirs
	rm onedee.o onedee pedant.o pedant

cleansubdirs:
	for X in $(SUBDIRS); do make -C $$X clean ;done
