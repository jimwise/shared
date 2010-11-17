#ifndef BLACKJACK_H
#define BLACKJACK_H

struct hand {
  double bet;	/* floats as money aren't a good idea, but close enough for this purpose */
  cardlist cards;
};

typedef struct hand hand;

hand hand_new (void);

void add (hand *, card);
int blackjack (hand *);
int busted (hand *);
int handvalue (hand *);
int *handvalues (hand *);
int hit (hand *);
void showvalue (hand *, int);
void muck (hand *);
void show (hand *, int);
void showvalue (hand *, int);

#endif
