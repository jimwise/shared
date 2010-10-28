#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nondeterminism.h"

/* Parlor Trick example (see _On Lisp_, sec, 22.2 (pp. 290-292) */
int
main (int argc, char **argv) {
  int x1, x2, sum;
  int ch[] = {5, 4, 3, 2, 1, 0};

  if (argc != 2) {
    fprintf(stderr, "usage: %s <num>\n", argv[0]);
    exit(1);
  }
  sum = atol(argv[1]);

  choose(x1, 6, ch);
  choose(x2, 6, ch);
  if ((x1 + x2) == sum) {
    printf("%d is the sum of %d and %d\n", sum, x1, x2);
  } else {
    fail();
  }
  exit(0);
}
