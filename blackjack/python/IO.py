#!/opt/csw/bin/python

from __future__ import print_function

import sys

last_bet = 5.00

def getbet(min, limit):
    global last_bet
    # XXX check actual min/table_limit rules
    print(("Please enter a bet (min = $%.2f, limit = $%.2f) [%.2f]: " %
           (min, limit, last_bet)), end=' ')
    while True:
        resp = sys.stdin.readline()[:-1]
        if resp == '':
            bet = last_bet
        else:
            if resp[0] == '$':
                roesp = resp[1:]
            try:
                bet = float(resp)
            except ValueError:
                print("Bet must be a number of dollars, try again: ", end=' ')
                continue

        if bet < min:
            print(("Bet must be at least $%.2f, try again: " % (min)), end=' ')
            continue
        if bet % min != 0:
            print(("Bet must be in an increment of $%d.00, try again: " % (min)), end=' ')
            continue
        if bet > limit:
            print(("Bet must be less than or equal to $%d.00, try again: " % (limit)), end=' ')
            continue

        # if we get here, bet is good
        last_bet = bet
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
