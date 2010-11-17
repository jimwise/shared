#!/opt/csw/bin/python

from __future__ import print_function

import sys

import Cards
import Blackjack
import IO

Cards.decksinshoe = 6

table_min = 5.00
table_limit = 1000.00

# XXX make configurable
player_purse = Blackjack.Purse(1000.00)

player_hand = Blackjack.PlayerHand()
dealer_hand = Blackjack.DealerHand()

def play_one_hand():
    global player_purse
    bet = IO.getbet(table_min, min(player_purse.purse, table_limit))
    player_purse.bet(bet)

    player_hand.deal();
    dealer_hand.deal();

    print()
    dealer_hand.show(False)
    print()
    player_hand.show()
    print("Bet: $%.2f" % player_purse.currbet)
    print()

    playersbest = player_hand.play(player_purse)
    if playersbest == 0:
        print("Dealer wins")
        return
    if player_hand.blackjack() and not dealer_hand.blackjack():
        print("Player wins")
        player_purse.blackjack()
        return

    print()

    dealersbest = dealer_hand.play();
    if dealersbest == 0:
        print("Player wins")
        player_purse.win()
        return
    
    print()
    dealer_hand.show(True)
    print()
    player_hand.show()
    print()

    if dealersbest > playersbest:
        print("Dealer wins")
        return
    elif playersbest > dealersbest:
        print("Player wins")
        player_purse.win()
        return
    else:
        print("Push")
        player_purse.push()
        return


if __name__ == "__main__":
    print("You have: $%.2f" % player_purse.purse)
    while True:
        player_hand.muck()
        dealer_hand.muck()

        play_one_hand()

        if player_purse.purse < table_min:
            print("You're out of money!")
            sys.exit(0)

        print("You have: $%.2f" % player_purse.purse)

        cont = IO.getresp(
            "Continue ([Y]es or [N]o) ([Y]N)? ",
            "Please anser [Y]es or [N]o (default Y): ",
            ["y", "n"], "y")
        if cont == "n":
            sys.exit(0)
