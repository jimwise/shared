#include <stdio.h>
#include <stdlib.h>
#include "cards.h"
#include "blackjack.h"

int
main (int argc, char **argv) {
  hand h;
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));
  printf("%s\n", card_name(draw()));

  h = hand_new();
  printf("Hit:\n");
  hit(&h);
  hit(&h);
  hit(&h);
  exit(0);
}
