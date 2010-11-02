#!/usr/pkg/bin/python2.4

import Cards
from functools import reduce

# yes, ace needs special treatment
vals = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10 ]

# may be clearer just to write out the dict
cardvals = dict(zip(Cards.cards, vals))

class Hand:
    cards = []
    dealer = False
    bet = 0
    
    def __init__(self, dealer = False):
        """create a new empty hand
        set 'dealer' to keep one card in the hole"""
        self.dealer = dealer

    def add(self, card):
        """add a card to hand"""
        self.cards.append(card)

    def muck(self):
        """discard all cards from a hand"""
        self.cards = []

    def hit(self):
        """hit a hand, and return new value()"""
        card = Cards.draw()
        print card.name()
        self.add(card)
        self.showvalue(reveal=True)
        if self.busted():
            print "[BUST]"
            return 0
        else:
            return self.value()

    def show(self, reveal=0):
        """pretty print a hand
        if a hand has a card in the hole, set 'reveal' to tru to show that card, otherwise
        card will be kept hidden"""
        # XXX use __repr__?  str?
        if self.cards == []:
            print "[no cards]"
            return
        else:
            if reveal or not self.dealer:
                print " ", self.cards[0].name()
            else:
                print "  one face down card"

            for card in self.cards[1:]:
                print " ", card.name()

        self.showvalue(reveal)

        if self.bet > 0:
            print "Bet: $%.2f" % self.bet

    def showvalue(self, reveal=False):
        """print value of a hand
        will show value as 'Total Value: ???' if hand has a card in the hole and 'reveal'
        is not set"""
        if self.dealer and not reveal:
            print "Total value: ???"
        else:
            print "Total value:", "/".join(map(str, self.values()))
    
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
        good = filter(lambda x: x<=21, self.values())
        if good == []:
            return 0

        return reduce(max, good)
