CXX?=c++
CXXFLAGS= -g --std=c++11 -Wall -Wextra -Wno-unused-parameter -Werror -Wpedantic

BIN= rpn
TESTBIN= test
OBJS= main.o
TESTOBJS= test.o
LIBOBJS= 
LIBS=
INCS= calc.hh ops.hh rpn.hh

all: $(BIN) $(TESTBIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

$(TESTBIN): $(TESTOBJS) $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

$(OBJS) $(TESTOBJS): $(INCS)

clean:
	rm -f $(BIN) test test.o $(OBJS) $(LIBOBJS)

$(OBJS): $(INCS)
