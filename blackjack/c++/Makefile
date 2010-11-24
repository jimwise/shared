# CXX= CC
# CXXFLAGS= -g -errwarn -v -errtags=yes
CXX= g++
CXXFLAGS= -g -Wall -Werror -pedantic
CCC= $(CXX)
CCCFLAGS= $(CXXFLAGS)
CPPFLAGS= -D__EXTENSIONS__

BIN= blackjack
OBJS= main.o
LIBOBJS= Cards.o Blackjack.o io.o
LIBS=
INCS= Cards.h Blackjack.h io.h

all: $(BIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

test: test.o $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) test test.o $(OBJS) $(LIBOBJS)

$(OBJS): $(INCS)
