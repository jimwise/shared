#!/usr/pkg/bin/python2.4

from __future__ import print_function
import random

decksinshoe = 6
 
suits = ['hearts', 'diamonds', 'clubs', 'spades']
cards = [ 'ace', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'jack', 'queen', 'king' ]

class Card:
    suit = None
    val = None

    def __init__(self, suit, val):
        self.suit = suit
        self.val = val

    def name (self):
        return self.val + " of " + self.suit

onedeck = [ Card(x, y) for x in suits for y in cards ]
shoe = []

def shuffle():
    for i in range(0,decksinshoe):
        shoe.extend(onedeck)

    # XXX XXX this is an ideal shuffle, which even a good shoe
    # doesn't provide...
    random.shuffle(shoe)

def draw():
    # a real shoe is reshuffled at a defined point before empty...
    if shoe == []:
        print("Refilling shoe with", decksinshoe, "decks")
        shuffle()

    return shoe.pop()
