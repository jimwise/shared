#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nondeterminism.h"

/* Choose 2 example  (see _On Lisp_, sec, 22.1 (pp. 286-289) */
int
main (int argc, char **argv) {
  int x;
  int ch[] = {2, 1};
  choose(x, 2, ch);
  if (x != 2) {
    fail();
  }
  printf("we chose: %d\n", x);
}
