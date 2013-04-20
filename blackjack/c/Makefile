CC=cc
# GCC / CLANG:
CFLAGS=-g -Wall -Wextra -Wno-unused-parameter -Werror -ansi -pedantic 
# Solaris CC:
# CFLAGS= -xc99=all -g -errwarn -Xc -v -xalias_level=strong -errtags=yes \
# 	-erroff=E_STATEMENT_NOT_REACHED
CPPFLAGS= -D__EXTENSIONS__

BIN= blackjack
OBJS= main.o 
LIBOBJS= io.o blackjack.o cards.o
LIBS=-lm
INCS= cards.h

all: $(BIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

test: test.o $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) test test.o $(OBJS) $(LIBOBJS)

$(OBJS): $(INCS)
