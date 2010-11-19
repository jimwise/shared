#include <stdio.h>
#include <stdlib.h>
#include "cards.h"
#include "blackjack.h"
#include "io.h"

#ifndef MIN
#define MIN(a, b) ((a) <= (b) ? (a) : (b))
#endif

/* using floats as money is a bad idea, but will do for here */
const double table_min = 5.00L;
const double table_limit = 1000.00L;

/* XXX make configurable */
static double player_purse = 1000.00L;

hand player_hand, dealer_hand;

void
showhands (int reveal) {
  printf("\nDealer has:\n");
  show(&dealer_hand, reveal);
  printf("\nPlayer has:\n");
  show(&player_hand, 1);
}

void
deal (void) {
  add(&player_hand, draw());
  add(&player_hand, draw());
  add(&dealer_hand, draw());
  add(&dealer_hand, draw());
  showhands(1);
}

int
playerplays (void) {
  int first = 1;
  double newbet;
  char action;

  /* XXX - insurance */
  if (blackjack(&player_hand)) {
    printf("[BLACKJACK]\n");
    return 21;
  }

  while(1) {			  /* actually, until we bust, surrender, or stand */
    /* XXX - split */
    /* XXX - this is `late surrender', early surrender has to be */
    /*        handled at insurance time, if it is to be offered */

    if (first) {
      action = getresp(
		       "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
		       "Please enter [H], [D], [S], or [U]: ",
		       "hdsu", 0);
    } else {
      action = getresp(
		       "[H]it or [S]tand (HS)? ",
		       "Please enter [H] or [S]: ",
		       "hs", 0);
    }

    switch (action) {
    case 'h':
      printf("You draw the ");
      if (!hit(&player_hand))
	return 0;
      /* XXX some casinos allow DD after split.  some don't (confirm) */
      first = 0;
      break;
    case 's':
      printf("You stand\n");
      return handvalue(&player_hand);
      /* NOTREACHED */
      break;
    case 'd':
      if (player_purse < table_min) {
	printf("You cannot afford to double down!\n");
	continue;
      }
      newbet = getbet(table_min, MIN(player_purse, table_limit));
      player_hand.bet += newbet;
      player_purse -= newbet;
      printf("You draw the ");
      return hit(&player_hand);
      break;
    case 'u':
      printf("You surrender\n");
      player_purse += 0.5 * player_hand.bet;
      return 0;
    }
  }
}

int
dealerplays (void) {
  if (blackjack(&dealer_hand)) {
    printf("[BLACKJACK]\n");
    return 21;
  }

  printf("The dealer reveals the %s\n", card_name(*(dealer_hand.cards.first)));
  showvalue(&dealer_hand, 1);

  while (handvalue(&dealer_hand) < 17) {
    /* XXX XXX should dealer hit a soft 17?  should this be configurable? */
    printf("Dealer draws the ");
    if (hit(&dealer_hand) == 0) {
      return 0;
    }
  }
  printf("Dealer stands\n");
  return handvalue(&dealer_hand);
}


void
play_one_hand(void) {
  int playersbest, dealersbest;

  player_hand.bet = getbet(table_min, MIN(player_purse, table_limit));
  player_purse -= player_hand.bet;

  deal();
  playersbest = playerplays();

  if (!playersbest) {
    printf("Dealer wins\n");
    return;
  }

  if (blackjack(&player_hand) && !blackjack(&dealer_hand)) {
    printf("Player wins\n");
    /* XXX XXX 3:2 (should this be configurable?) on blackjack */
    player_purse += 2.5 * player_hand.bet;
    return;
  }

  printf("\n");

  dealersbest = dealerplays();
  if (!dealersbest) {
    printf("Player wins\n");
    player_purse += 2 * player_hand.bet;
    return;
  }

  printf("\n");
  showhands(1);

  if (dealersbest > playersbest) {
    printf("Dealer wins\n");
    return;
  } else if (playersbest > dealersbest) {
    printf("Player wins\n");
    player_purse += 2 * player_hand.bet;
    return;
  } else {
    printf("Push\n");
    player_purse += player_hand.bet;
    return;
  }
}

int
main (int argc, char **argv) {
  char c;
  player_hand = hand_new();
  dealer_hand = hand_new();
  printf("You have: $%0.2f\n", player_purse);
  while (1) {
    muck(&player_hand);
    muck(&dealer_hand);

    play_one_hand();

    if (player_purse < table_min) {
      printf("You're out of money!\n");
      exit(0);
    }

    printf("You have: $%0.2f\n", player_purse);

    c = getresp("Continue ([Y]es or [N]o) ([Y]N)? ",
		"Please anser [Y]es or [N]o (default Y): ",
		"yn", 'y');
    if (c == 'n')
      exit(0);
  }
}
