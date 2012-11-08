#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "numstack.h"
#include "op.h"
#include "ops.h"

void
act (stack st, char *s) {
  op o = lookup(dict, s);
  if (op_found(o)) {
    operate(st, o);
  } else {
    char *end;
    num n = strtod(s, &end);
    if (end == s) { /* no conversion possible */
      signal_error("Unknown Operation: %s", s);
    } else {
      push(st, n);
    }
  }
}

int
main (int argc, char **argv) {
  char *ln=NULL;
  size_t lnc=0, len;
  stack st = make_stack();

  printf("> ");
  while ((len = getline(&ln, &lnc, stdin)) != -1) {
    if (len == 0)
      continue;
    if (ln[len-1] == '\n')
      ln[len-1] = '\0';

    act(st, ln);

    printf("> ");
  }
}
