#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nondeterminism.h"

/* Chocoblob Coin Search example (with cuts) (see _On Lisp_, sec, 22.5 (pp. 298-302) */
int
coinp (char *city, int store, int box) {
  return((!strcmp(city, "la") && store == 1 && box == 2) ||
	 (!strcmp(city, "ny") && store == 1 && box == 1) ||
	 (!strcmp(city, "bos") && store == 2 && box == 2));
}

int
main (int argc, char **argv) {
  char *city;
  char *cities[] = {"bos", "ny", "la"};
  int box, store;
  int oneortwo[] = {2, 1};

  choose(city, 3, cities);
  /* mark(); */
  choose(store, 2, oneortwo);
  choose(box, 2, oneortwo);
  printf("(%s %d %d) ", city, store, box);
  if (coinp(city, store, box)) {
    /* cut(); */
    printf("C ");
  }
  fail();
  printf("\n");
  exit(0);
}
