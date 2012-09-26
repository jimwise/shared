#include <algorithm>
#include <deque>
#include <iostream>
#include <string>
#include <vector>

#include "Cards.h"

using namespace std;

namespace {
  const string suit_names[] = {"Hearts", "Diamonds", "Clubs", "Spades"};
  const string val_names[] = {"Ace", "Two", "Three", "Four", "Five", "Six", "Seven",
				      "Eight", "Nine", "Ten", "Jack", "Queen", "King"};
}

string
Card::name (void) {
  return val_names[va] + " of " + suit_names[su];
}

Shoe::Shoe (int d) {
  decksinshoe = d;
}

Shoe::~Shoe (void) {
  vector<Card *>::iterator cx;
  for (cx = onedeck.begin(); cx != onedeck.end(); ++cx) {
    delete *cx;
  }
}

Card *
Shoe::draw (void) {
  if (shoe.empty())
    refill();

  Card *c = shoe.front();
  shoe.pop_front();
  return c;
}

// This is annoying.  The default RNG for STL cannot be seeded in a portable way
// so we have to provide our own.  Grr.

class ShoeRNG {
public:
  ShoeRNG (void) {srand48(time(NULL));}
  int operator() (const int &n) {return lrand48() % n;}
};

namespace {
  ShoeRNG sr;
}

void
Shoe::refill (void) {
  if (onedeck.empty()) {
    // here's how this works for memory allocation -- we new up onedeck, and then multiple
    // pointers to the same card float around as needed.
    for (Card::suit s=Card::HEARTS; s<=Card::SPADES; s++)
      for (Card::val v=Card::ACE; v<=Card::KING; v++)
	onedeck.push_back(makeCard(s, v));
  }

  if (!shoe.empty())
    shoe.clear();

  cout << "Refilling shoe with " << decksinshoe << " decks" << endl;
  for (int i=0; i<decksinshoe; i++) {
    copy(onedeck.begin(), onedeck.end(), front_insert_iterator<deque<Card *> >(shoe));
  }
  random_shuffle(shoe.begin(), shoe.end(), sr);
}
