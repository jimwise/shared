#ifndef BLACKJACK_H
#define BLACKJACK_H

#include <vector>
#include "Cards.h"

class BlackjackCard : public Card {
public:
  BlackjackCard (Card::suit s, Card::val v) : Card(s, v) {}
  int value (void);
};
  
class BlackjackShoe : public Shoe {
public:
  BlackjackShoe (int d) : Shoe(d) {}
protected:
  Card *makeCard (Card::suit s, Card::val v) {return new BlackjackCard(s, v);}
};

class Purse {
public:
  Purse (double stake, double min, double limit) {
    purse = stake; table_min = min; table_limit = limit; currbet = 0.0;
  }
  void bet (double b) {purse -= (currbet = b);}
  void doubledown (double b) {bet(b);}
  /* XXX XXX 3:2 (should this be configurable?) on blackjack */
  void blackjack (void) {purse += 2.5 * currbet;}
  void surrender (void) {purse += 0.5 * currbet;}
  void win (void) {purse += 2.0 * currbet;}
  void push (void) {purse += currbet;}
  double getPurse (void) {return purse;}
  double getBet (void) {return currbet;}
  double getMin (void) {return table_min;}
  double getLimit (void) {return table_limit;}
private:
  double purse, currbet, table_min, table_limit;
};

class Hand {
public:
  Hand (Shoe *s);
  void deal (void);
  void add (Card *c);
  void muck (void);

  bool busted (void);
  bool blackjack (void);
  int value (void);

  virtual int hit (void);
  virtual int play (void) = 0;
  virtual void show (bool reveal);

protected:
  Shoe *shoe;
  vector<Card *> cards;

  vector<int> values (void);
  void showvalue (bool reveal);
};

class PlayerHand : public Hand {
public:
  PlayerHand (Shoe *s, Purse *p) : Hand(s) {purse = p;}
  virtual int hit (void);
  virtual int play (void);
  virtual void show (bool reveal);
private:
  Purse *purse;
};

class DealerHand : public Hand {
public:
  DealerHand (Shoe *s) : Hand(s) {}
  virtual int hit (void);
  virtual int play (void);
  virtual void show (bool reveal);
};

#endif
