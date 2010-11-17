#ifndef CARDS_H
#define CARDS_H

enum suit{HEARTS=0, DIAMONDS, CLUBS, SPADES};
enum value {ACE=0, TWO, THREE, FOUR, FIVE, SIX, SEVEN,
	    EIGHT, NINE, TEN, JACK, QUEEN, KING};

struct card {
  enum suit suit;
  enum value value;
  struct card *next;		/* linkage within hand */
};

struct cardlist {
  struct card *first;
  struct card *last;
};

typedef struct card card;
typedef struct cardlist cardlist;

const char *card_name (card);
card draw (void);

cardlist clist_new (void);
void clist_add (cardlist *, card);
int clist_length (cardlist);
void clist_clear (cardlist *);

#endif
