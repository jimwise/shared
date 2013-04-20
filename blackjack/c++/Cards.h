#ifndef CARDS_H
#define CARDS_H

#include <deque>
#include <string>
#include <vector>

using namespace std;

class Card {
public:
  enum _suit {HEARTS=0, DIAMONDS, CLUBS, SPADES};
  enum _value {ACE=0, TWO, THREE, FOUR, FIVE, SIX, SEVEN,
	       EIGHT, NINE, TEN, JACK, QUEEN, KING};
  typedef unsigned int suit;
  typedef unsigned int val;

  Card (suit s, val v) : su(s), va(v) {}
  string name (void);
  virtual int value (void) {return 0;};
  virtual ~Card (void) {}
protected:
  suit su;
  val va;
};

class Shoe {
public:
  Shoe (int d = 6);
  Card *draw (void);
  ~Shoe (void);

protected:
  virtual Card *makeCard (Card::suit s, Card::val v) {return new Card(s, v);}

private:
  int decksinshoe;

  vector<Card *> onedeck;
  deque<Card *> shoe;

  void refill (void);
};

#endif
