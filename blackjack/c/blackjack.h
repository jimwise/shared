#ifndef BLACKJACK_H
#define BLACKJACK_H

#include "cards.h"

struct hand {
  double bet;	/* floats as money aren't a good idea, but close enough for this purpose */
  cardlist cards;
};

typedef struct hand hand;

hand hand_new (void);

void add (hand *, card);
int blackjack (const hand *);
int busted (const hand *);
int handvalue (const hand *);
int *handvalues (const hand *);
int hit (hand *);
void muck (hand *);
void show (hand *, int);
void showvalue (const hand *, int);

#endif
