#include <cstdlib>
#include <iostream>

#include "Cards.h"
#include "Blackjack.h"

using namespace std;

int
main (int argc, char **argv) {
  BlackjackShoe s(6);
  Purse pu(1000.00, 5.00, 1000.00);

  PlayerHand p(&s, &pu);
  p.hit();
  p.hit();
  p.hit();
  p.show(true);

  DealerHand d(&s);
  d.deal();
  d.show(false);
  d.play();
  d.show(true);

  exit(0);
}
