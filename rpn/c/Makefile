CC?= cc
CFLAGS= -g --std=c11 -Wall -Werror -Wpedantic

BIN= rpn
TESTBIN=
OBJS= rpn.o
TESTOBJS=
LIBOBJS= numstack.o op.o ops.o
LIBS= -lm
INCS= numstack.h op.h ops.h

all: $(BIN) $(TESTBIN)

$(BIN): $(OBJS) $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

$(TESTBIN): $(TESTOBJS) $(LIBOBJS)
	$(CC) -o $@ $^ $(LIBS)

$(OBJS) $(TESTOBJS): $(INCS)

clean:
	rm -f $(BIN) $(TESTBIN) $(OBJS) $(TESTOBJS) $(LIBOBJS)

$(OBJS) $(TESTOBJS) $(LIBOBJS): $(INCS)
