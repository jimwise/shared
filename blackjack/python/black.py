#!/usr/pkg/bin/python2.4

import sys

import Cards
import Blackjack

Cards.decksinshoe = 6

table_min = 5
table_limit = 1000

# XXX make configurable
player_purse = 1000.00
player_last_bet = table_min

player_hand = Blackjack.Hand()
dealer_hand = Blackjack.Hand(dealer=True)

def deal():
    player_hand.add(Cards.draw())
    player_hand.add(Cards.draw())
    dealer_hand.add(Cards.draw())
    dealer_hand.add(Cards.draw())
    show_hands()

def show_hands(reveal=0):
    print("Dealer has:")
    dealer_hand.show(reveal)
    print()
    print("Player has:")
    player_hand.show()

def playerplays():
    global player_purse
    first_draw = True
    # XXX - insurance
    if player_hand.blackjack():
        print("[BLACKJACK]")
        return 21
    
    while True:                    # actually, until we bust or stand
        # XXX - split
        # XXX - this is `late surrender', early surrender has to be
        # handled at insurance time, if it is to be offered
        if first_draw:
            action = getresp(
                "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
                "Please enter [H], [D], [S], or [U]: ",
                ["h", "d", "s", "u"], "" )
        else:
            action = getresp(
                "[H]it or [S]tand (HS)? ",
                "Please enter [H] or [S]: ",
                ["h", "s"], "")

        if action == "h":
            print("You draw the", end=' ')
            if player_hand.hit() == 0:
                return 0
            # XXX some casinos allow DD after split.  some don't (confirm)
            first_draw = False
        elif action == "s":
            print("You stand")
            return player_hand.value()
        elif action == "d":
            if player_purse < table_min:
                print("You cannot afford to double down!")
                continue
            newbet = getbet(table_min, player_hand.bet)
            player_hand.bet += newbet
            player_purse -= newbet
            print("You draw the", end=' ')
            return player_hand.hit()
        elif action == "u":
            print("You surrender.")
            player_purse += 0.5 * player_hand.bet
            return 0

def dealerplays():
    if dealer_hand.blackjack():
        print("[BLACKJACK]")
        return 21

    print("The dealer reveals the", dealer_hand.cards[0].name())
    dealer_hand.showvalue(reveal=True)

    while True:
        # XXX XXX should dealer hit a soft 17?  should this be configurable?
        if dealer_hand.value() < 17:
            print("Dealer draws the", end=' ')
            if dealer_hand.hit() == 0:
                return 0
        else:
            print("Dealer stands")
            return dealer_hand.value()

def play_one_hand():
    global player_purse
    player_hand.bet = getbet(table_min, table_limit)
    player_purse -= player_hand.bet

    deal()
    playersbest = playerplays()
    if playersbest == 0:
        print("Dealer wins")
        return
    if player_hand.blackjack() and not dealer_hand.blackjack():
        print("Player wins")
        # XXX XXX 3:2 (configurable) on blackjack
        player_purse += 2.5 * player_hand.bet
        return

    print()

    dealersbest = dealerplays()
    if dealersbest == 0:
        print("Player wins")
        player_purse += 2 * player_hand.bet
        return
    
    print()
    show_hands(reveal=True)
    print()

    if dealersbest > playersbest:
        print("Dealer wins")
        return
    elif playersbest > dealersbest:
        print("Player wins")
        player_purse += 2 * player_hand.bet
        return
    else:
        print("Push")
        player_purse += player_hand.bet
        return


def getbet(table_min, table_limit):
    global player_last_bet, player_purse
    # XXX check actual min/table_limit rules
    print(("Please enter a bet (min = $%.2f, limit = $%.2f) [%.2f]: " % (table_min, min(player_purse, table_limit), player_last_bet)), end=' ')
    while True:
        resp = sys.stdin.readline()[:-1]
        if resp == '':
            bet = player_last_bet
        else:
            if resp[0] == '$':
                resp = resp[1:]
            try:
                bet = float(resp)
            except ValueError:
                print("Bet must be a number of dollars, try again: ", end=' ')
                continue

        if bet < table_min:
            print("Bet must be at least $%.2f, try again: ", end=' ')
            continue
        if bet % table_min != 0:
            print("Bet must be in an increment of $%d.00, try again: " % min, end=' ')
            continue
        if bet > table_limit:
            print("Bet must be less than or equal to $%d.00, try again: " % table_limit, end=' ')
            continue

        # if we get here, bet is good
        player_last_bet = bet
        return bet

def getresp(prompt1, prompt2, allowed, default):
    print(prompt1, end=' ')
    while True:
        resp = sys.stdin.readline()
        if resp[-1] == "\n":
            resp = resp[:-1]
        resp = resp.lower()
        if resp in allowed:
            return resp
        if resp == '' and default != '':
            return default
        print(prompt2, end=' ')

if __name__ == "__main__":
    print("You have: $%.2f" % player_purse)
    while True:
        player_hand.muck()
        dealer_hand.muck()

        play_one_hand()

        if player_purse < table_min:
            print("You're out of money!")
            sys.exit(0)

        print("You have: $%.2f" % player_purse)

        cont = getresp(
            "Continue ([Y]es or [N]o) ([Y]N)? ",
            "Please anser [Y]es or [N]o (default Y): ",
            ["y", "n"], "y")
        if cont == "n":
            sys.exit(0)
