#include <stdio.h>

#include "cards.h"
#include "blackjack.h"

const int cardvals[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10};

hand
hand_new (void) {
  hand h;
  h.bet = 0;
  h.cards = clist_new();
  return h;
}

int
cardval (card c) {
  return (cardvals[c.value]);
}

void
add (hand *h, card c) {
  clist_add(&h->cards, c);
}

void
muck (hand *h) {
  clist_clear(&(h->cards));
}

int
hit (hand *h) {
  card c = draw();
  printf("%s\n", card_name(c));
  add(h, c);
  showvalue(h, 1);
  if (busted(h)) {
    printf("[BUST]\n");
    return 0;
  } else {
    return handvalue(h);
  }
}

void
show (hand *h, int reveal) {
  card *c;
  if (clist_length(h->cards) == 0) {
    printf("[no cards]\n");
    return;
  } else {
    if (reveal) {
      printf(" %s\n", card_name(*h->cards.first));
    } else {
      printf(" one face down card\n");
    }
  }
  for (c=h->cards.first->next; c!=NULL; c=c->next)
    printf(" %s\n", card_name(*c));

  showvalue(h, reveal);
  if (h->bet > 0)
    printf ("Bet: $%0.2f\n", h->bet);
}

void
showvalue (hand *h, int reveal) {
  int *vs;
  if (reveal) {
    printf("Total value: ");
    vs = handvalues(h);
    printf("%d", *(vs++));
    while (*vs != -1)
      printf(" / %d", *(vs++));
    printf ("\n");
  } else {
    printf("Total value: ???\n");
  }
}

int
busted (hand *h) {
  return handvalue(h) == 0;
}

int
blackjack (hand *h) {
  return (clist_length(h->cards) == 2) && (handvalue(h) == 21);
}

/* 2*(max no. of cards to bust) (imagine pathological case of 22 aces (of 24 in 6-card shoe) */
static int handvalbuf[44];

int *
handvalues (hand *h) {
  int val = 0, aces = 0;
  int i, j, k, t, p, oldp;
  card *c;

  for (c = h->cards.first; c != NULL; c = c->next) {
    if (cardval(*c) == 1)
      aces++;
    else
      val += cardval(*c);
  }

  handvalbuf[0] = val;
  handvalbuf[1] = -1;
  oldp = p = 1;

  for (i=0; i<aces; i++) {
    for (j = 0; j<oldp; j++) {
      t = handvalbuf[j];
      handvalbuf[j] += 1;
      handvalbuf[p++] = t + 11;
    }
    handvalbuf[p] = -1;
    oldp = p;
  }

  /* merge duplicates */
  for (i=0; handvalbuf[i]!=-1; i++) {
    for (j=i+1; handvalbuf[j]!=-1; j++) {
      if (handvalbuf[j] == handvalbuf[i]) {
	for (k=j; handvalbuf[k]!=-1; k++) {
	  handvalbuf[k] = handvalbuf[k+1];
	}
	handvalbuf[k] = -1;
      }
    }
  }
  return handvalbuf;
}

int
handvalue (hand *h) {
  int *vs;
  int v = 0;
  vs = handvalues(h);
  while (*vs != -1) {
    if ((*vs <= 21) && (*vs > v))
      v = *vs;
    vs++;
  }
  return v;
}
