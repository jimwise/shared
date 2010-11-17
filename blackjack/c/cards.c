#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "cards.h"

const int decksinshoe = 6;
const int cardsindeck = 52;
const int cardsinshoe = 6 * 52;
static int currcard;

const char *suit_names[] = {"Hearts", "Diamonds", "Clubs", "Spades"};
const char *value_names[] = {"Ace", "Two", "Three", "Four", "Five", "Six", "Seven",
			     "Eight", "Nine", "Ten", "Jack", "Queen", "King"};

static struct card *onedeck = NULL;
static struct card *shoe = NULL;

static void knuth_shuffle_cards (void);

/* uses a static buffer; not re-entrant */
static char cname[17];

const char *
card_name (card c) {
  snprintf(cname, sizeof(cname), "%s of %s", value_names[c.value], suit_names[c.suit]);
  return cname;
}

static void
init_onedeck (void) {
  enum suit s;
  enum value v;
  int i = 0;

  /* catch attempts to reinit onedeck */
  assert(onedeck == NULL);

  /* never freed; generated once */
  onedeck = malloc(cardsindeck * sizeof(struct card));
  for (s=HEARTS; s<=SPADES; s++)
    for (v=ACE; v<=KING; v++) {
      onedeck[i].suit = s;
      onedeck[i].value = v;
      i++;
    }
}

card
draw (void) {
  int i, j, k;

  if (shoe == NULL) {
    shoe = malloc(cardsinshoe * sizeof(card));
    srand48(time(NULL));
    currcard = cardsinshoe;

    /* only need this once; afterward, we shuffle the same cards again and again */
    if (onedeck == NULL)
      init_onedeck();
    for (i=0, k=0; i<decksinshoe; i++)
      for (j=0; j<cardsindeck; j++, k++)
	shoe[k] = onedeck[j];
  }

  if (currcard == cardsinshoe) {
    printf("Refilling shoe with %d decks\n", decksinshoe);
    knuth_shuffle_cards();
    currcard = 0;
  }

  return shoe[currcard++];
}

/* adapted from scheme version in ../scheme, see there for attribution */
void
knuth_shuffle_cards (void) {
  int i, r;
  struct card t;

  for (i=cardsinshoe; i>0; i--) {
    r = lrand48() % cardsinshoe;
    t = shoe[r];
    shoe[r] = shoe[i];
    shoe[i] = t;
  }
}

cardlist
clist_new (void) {
  cardlist cs = {NULL, NULL};
  return cs;
}

void
clist_add (cardlist *cs, card c) {
  card *cn;

  assert(cs != NULL);
  cn = malloc(sizeof(card)); /* freed in clist_free */
  assert(cn != NULL);

  *cn = c;

  if (cs->first == NULL) {
    cs->first = cs->last = cn;
  } else {
    cs->last->next = cn;
    cn->next = NULL;
    cs->last = cn;
  }
}

int
clist_length (cardlist cs) {
  int n = 0;
  card *c;

  for (c = cs.first; c != NULL; c = c->next)
    n++;

  return n;
}

void
clist_clear (cardlist *cs) {
  card *c;
  for (c = cs->first; c!=NULL; c = c->next)
    free(c);

  cs->first = cs->last = NULL;
}
