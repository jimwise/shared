CXX= CC
CXXFLAGS= -g -errwarn -v -errtags=yes
# CXX= g++
# CXXFLAGS= -g -Wall -Werror -pedantic
CCC= $(CXX)
CCCFLAGS= $(CXXFLAGS)
CPPFLAGS= -D__EXTENSIONS__

BIN= rpn
OBJS= main.o
LIBOBJS= 
LIBS=
INCS= calc.hh ops.hh

all: $(BIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

test: test.o $(LIBOBJS)
	$(CXX) -o $@ $^ $(LIBS)

clean:
	rm -f $(BIN) test test.o $(OBJS) $(LIBOBJS)

$(OBJS): $(INCS)
