#include <stdio.h>
#include <stdlib.h>

#include "nondeterminism.h"


int
choose_2 (void) {
  int x;
  int ch[2] = {2, 1};
  CHOOSE(x, 2, ch);
  if (x == 1) {
    FAIL();
  }
  printf("we chose: %d\n", x);
}

int
parlor_trick (int sum) {
  int x1, x2;
  int ch[6] = {0, 1, 2, 3, 4, 5};
  CHOOSE(x1, 6, ch);
  CHOOSE(x2, 6, ch);
  if ((x1 + x2) == sum)
    printf("%d is the sum of %d and %d\n", sum, x1, x2);
  else
    FAIL();
}

int
main (int argc, char **argv) {
  ND_INIT;
  choose_2();

  parlor_trick(7);
  exit(0);
}
