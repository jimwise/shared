#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nondeterminism.h"

/* Choose 2 example  (see _On Lisp_, sec, 22.1 (pp. 286-289) */
void
choose_2 (void) {
  int x;
  int ch[] = {2, 1};
  choose(x, 2, ch);
  if (x != 2) {
    fail();
  }
  printf("we chose: %d\n", x);
}

/* Parlor Trick example (see _On Lisp_, sec, 22.2 (pp. 290-292) */
void
parlor_trick (int sum) {
  int x1, x2;
  int ch[] = {5, 4, 3, 2, 1, 0};
  choose(x1, 6, ch);
  choose(x2, 6, ch);
  if ((x1 + x2) == sum) {
    printf("%d is the sum of %d and %d\n", sum, x1, x2);
  } else {
    fail();
  }
}

/* Chocoblob Coin Search example (with cuts) (see _On Lisp_, sec, 22.5 (pp. 298-302) */
int
coinp (char *city, int store, int box) {
  return((!strcmp(city, "la") && store == 1 && box == 2) ||
	 (!strcmp(city, "ny") && store == 1 && box == 1) ||
	 (!strcmp(city, "bos") && store == 2 && box == 2));
}

void
find_boxes (void) {
  char *city;
  char *cities[] = {"bos", "ny", "la"};
  int box, store;
  int oneortwo[] = {2, 1};

  choose(city, 3, cities);
  mark();
  choose(store, 2, oneortwo);
  choose(box, 2, oneortwo);
  printf("(%s %d %d) ", city, store, box);
  if (coinp(city, store, box)) {
    cut();
    printf("C ");
  }
  fail();
  printf("\n");
}

int
main (int argc, char **argv) {
  choose_2();
  nd_reset();
  parlor_trick(7);
  nd_reset();
  find_boxes();
  exit(0);
}
