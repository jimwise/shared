#!/opt/csw/bin/python

from __future__ import print_function
from functools import reduce

import Cards
import IO

# yes, ace needs special treatment
vals = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10 ]

# may be clearer just to write out the dict
cardvals = dict(list(zip(Cards.cards, vals)))

class Hand:
    cards = []
    dealer = False
    
    def deal  (self):
        self.add(Cards.draw());
        self.add(Cards.draw());

    def add(self, card):
        """add a card to hand"""
        self.cards.append(card)

    def muck(self):
        """discard all cards from a hand"""
        self.cards = []

    def hit(self):
        """hit a hand, and return new value()"""
        card = Cards.draw()
        print(card.name())
        self.add(card)
        self.showvalue(reveal=True)
        if self.busted():
            print("[BUST]")
            return 0
        else:
            return self.value()

    def show(self, reveal=0):
        """pretty print a hand
        if a hand has a card in the hole, set 'reveal' to tru to show that card, otherwise
        card will be kept hidden"""
            
        # XXX use __repr__?  str?
        if self.cards == []:
            print("[no cards]")
            return
        else:
            if reveal or not self.dealer:
                print(" ", self.cards[0].name())
            else:
                print("  one face down card")

            for card in self.cards[1:]:
                print(" ", card.name())

        self.showvalue(reveal)


    def showvalue(self, reveal=False):
        """print value of a hand
        will show value as 'Total Value: ???' if hand has a card in the hole and 'reveal'
        is not set"""
        if self.dealer and not reveal:
            print("Total value: ???")
        else:
            print("Total value:", "/".join(map(str, list(self.values()))))
    
    def busted(self):
        """return True if a hand is busted"""
        return self.value() == 0

    def blackjack(self):
        """return true if a hand is blackjack"""
        return len(self.cards) == 2 and self.value() == 21

    def values(self):
        """return all possible values for a hand, considering aces"""
        val = 0
        aces = 0
        for card in self.cards:
            if cardvals[card.val] == 1:
                aces+=1
            else:
                val += cardvals[card.val]

            handval = [val]

            for ace in range(aces):
                newhandval = []
                for i in handval:
                    if newhandval.count(i+1) == 0:
                        newhandval.append(i + 1)
                    if not newhandval.count(i+1) == 0:
                        newhandval.append(i + 11)
                handval = newhandval

        return handval

    def value(self):
        """return the best non-busted value for a hand, considering aces
        returns zero if hand is busted"""
        good = [x for x in list(self.values()) if x<=21]
        if good == []:
            return 0

        return reduce(max, good)

class DealerHand(Hand):
    Dealer = True;

    def show(self, reveal=0):
        print("Dealer has:")
        Hand.show(self, reveal);

    def play(self):
        if self.blackjack():
            print("[BLACKJACK]")
            return 21

        print("The dealer reveals the", self.cards[0].name())
        self.showvalue(reveal=True)

        while True:
            # XXX XXX should dealer hit a soft 17?  should this be configurable?
            if self.value() < 17:
                print("Dealer draws the", end=' ')
                if self.hit() == 0:
                    return 0
            else:
                print("Dealer stands")
                return self.value()


class PlayerHand(Hand):
    Dealer = False;

    def show(self, reveal=1):
        print("Player has:")
        Hand.show(self, reveal);

    def play(self, purse):
        first_draw = True
        # XXX - insurance
        if self.blackjack():
            print("[BLACKJACK]")
            return 21
    
        while True:                    # actually, until we bust, surrender or stand
            # XXX - split
            # XXX - this is `late surrender', early surrender has to be
            # handled at insurance time, if it is to be offered
            if first_draw:
                action = IO.getresp(
                    "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
                    "Please enter [H], [D], [S], or [U]: ",
                    ["h", "d", "s", "u"], "" )
            else:
                action = IO.getresp(
                    "[H]it or [S]tand (HS)? ",
                    "Please enter [H] or [S]: ",
                    ["h", "s"], "")

            if action == "h":
                print("You draw the", end=' ')
                if self.hit() == 0:
                    return 0
                # XXX some casinos allow DD after split.  some don't (confirm)
                first_draw = False
            elif action == "s":
                print("You stand")
                return self.value()
            elif action == "d":
                if purse.purse < table_min:
                    print("You cannot afford to double down!")
                    continue
                newbet = IO.getbet(table_min, min(purse.currbet, table_limit))
                purse.doubledown(newbet)
                print("You draw the", end=' ')
                return self.hit()
            elif action == "u":
                print("You surrender")
                purse.surrender()
                return 0

class Purse:
    purse = 0.0
    currbet = 0.0

    def __init__ (self, stake):
        self.purse = stake

    def bet (self, b):
        self.currbet = b
        self.purse -= self.currbet

    def doubledown (self, b):
        self.currbet += b

    # XXX XXX 3:2 (configurable) on blackjack
    def blackjack (self):
        self.purse += 2.5 * self.currbet

    def surrender (self):
        self.purse += 0.5 * self.currbet

    def win (self):
        self.purse += 2 * self.currbet

    def push (self):
        self.purse += self.currbet
