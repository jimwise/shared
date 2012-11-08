#include <stdarg.h>
#include <stdio.h>
#include <strings.h>
#include "op.h"

void
signal_error (char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
}

void
operate (stack s, op o) {
  if (size(s) < o.arity) {
    signal_error("Stack Underflow");
  } else {
    o.act(s);
  }
}

op
lookup (opdict o, char *s) {
  int n=0;
  while (op_found(o[n]) && strcasecmp(s, o[n].name))
    n++;
  return o[n];
}

int
op_found (op o) {
  return o.act != NULL;
}
