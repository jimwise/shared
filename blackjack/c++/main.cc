#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <iomanip>

#include "Cards.h"
#include "Blackjack.h"
#include "io.h"


namespace {
  // using floats as money is a bad idea, but will do for here

  // XXX make configurable
  const int decksInShoe = 6;
  const double tableMin = 5.00L;
  const double tableLimit = 1000.00L;
  const double playerStake = 1000.00L;

  BlackjackShoe shoe(decksInShoe);
  Purse playerPurse(playerStake, tableMin, tableLimit);
  DealerHand dealerHand(shoe);
  PlayerHand playerHand(shoe, playerPurse);
}

void
playOneHand(void) {
  int playersbest, dealersbest;

  playerPurse.bet(getbet(tableMin, min(playerPurse.getPurse(), tableLimit)));

  playerHand.deal();
  dealerHand.deal();

  cout << endl;
  dealerHand.show(false);
  cout << endl;
  playerHand.show(true);
  cout << "Bet: "<< playerPurse.getBet() << endl;
  cout << endl;

  playersbest = playerHand.play();

  if (!playersbest) {
    cout << "Dealer wins" << endl;
    return;
  }

  if (playerHand.blackjack() && !dealerHand.blackjack()) {
    cout << "Player wins" << endl;
    playerPurse.blackjack();
    return;
  }

  cout << endl;

  dealersbest = dealerHand.play();
  if (!dealersbest) {
    cout << "Player wins" << endl;
    playerPurse.win();
    return;
  }

  cout << endl;
  dealerHand.show(false);
  cout << endl;
  playerHand.show(true);
  cout << endl;

  if (dealersbest > playersbest) {
    cout << "Dealer wins" << endl;
    return;
  } else if (playersbest > dealersbest) {
    cout << "Player wins" << endl;
    playerPurse.win();
    return;
  } else {
    cout << "Push" << endl;
    playerPurse.push();
    return;
  }
}

int
main (int argc, char **argv) {
  cout << fixed << setprecision(2);
  cout << "You have: $" << playerPurse.getPurse() << endl;

  while (1) {
    playerHand.muck();
    dealerHand.muck();

    playOneHand();

    if (playerPurse.getPurse() < tableMin) {
      cout << "You're out of money!" << endl;
      exit(0);
    }

    cout << "You have: $" << playerPurse.getPurse() << endl;

    if (getresp("Continue ([Y]es or [N]o) ([Y]N)? ",
		"Please anser [Y]es or [N]o (default Y): ",
		"yn", 'y') == 'n')
      exit(0);
  }
}
