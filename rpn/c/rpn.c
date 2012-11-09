#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
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
  char buf[128];
  stack st = make_stack();

  printf("> ");
  while (fgets(buf, sizeof(buf), stdin) != NULL) {
    char *nl;
    if ((nl = strchr(buf, '\n')) != NULL)
      *nl = '\0';
    if (strlen(buf) == 0)
      continue;

    act(st, buf);

    printf("> ");
  }
}
