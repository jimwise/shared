#include <math.h>
#include <stdio.h>
#include "ops.h"

#define MAKE_NULLARY_ACTION(name, code)	\
  void _ ## name ## _action (stack s) { code }

#define MAKE_UNARY_ACTION(name, code) \
  void _ ## name ## _action (stack s) { num x = pop(s); code }

#define MAKE_BINARY_ACTION(name, code) \
  void _ ## name ## _action (stack s) { num y = pop(s); num x = pop(s); code }

#define make_op(name, key, doc, arity) { key, doc, arity, _ ## name ## _action }

MAKE_NULLARY_ACTION(size, printf("%zd\n", size(s)); )
MAKE_UNARY_ACTION(show, printf("%f\n", x); push(s, x); )

MAKE_BINARY_ACTION(plus, push(s, x+y); )
MAKE_BINARY_ACTION(minus, push(s, x-y); )
MAKE_BINARY_ACTION(times, push(s, x*y); )
MAKE_BINARY_ACTION(divby, push(s, x/y); )
MAKE_BINARY_ACTION(expt, push(s, pow(x, y)); )

MAKE_UNARY_ACTION(drop, (void)x;) /* cast silences unused parameter warning more-or-less portably */
MAKE_UNARY_ACTION(dup, push(s, x); push(s, x);)
MAKE_BINARY_ACTION(swap, push(s, y); push(s, x);)

void
_help_action (stack s) {
  size_t n = 0;
  for (op *o = dict; op_found(*o); o++) {
    n++;
  }
  printf("%zd Commands:\n", n);
  for (op *o = dict; op_found(*o); o++) {
    printf("%s -- %s\n", o->name, o->doc);
  }
}

/* must end with op_unknown */
opdict dict = {
  make_op(show, ".", "display top value on the stack", 1),
  make_op(size, "#", "display number of values on the stack", 0),

  make_op(plus, "+", "replace top two values on the stack with their sum", 2),
  make_op(minus, "-", "replace top two values on the stack with their sum", 2),
  make_op(times, "*", "replace top two values on the stack with their sum", 2),
  make_op(divby, "/", "replace top two values on the stack with their sum", 2),
  make_op(expt, "^", "replace top two values on the stack, x and y, with x to the yth power", 2),

  make_op(drop, "drop", "remove top value from the stack", 1),
  make_op(dup, "dup", "duplicate top value on the stack", 1),
  make_op(swap, "swap", "swap top two values on the stack", 2),

  make_op(help, "help", "show this help", 0),

  op_unknown
};
