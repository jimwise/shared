CXX?=c++
CXXFLAGS= -g --std=c++11 -Wall -Wextra -Wno-unused-parameter -Werror -Wpedantic

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
