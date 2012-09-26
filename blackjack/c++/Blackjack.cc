#include <algorithm>
#include <iostream>
#include <vector>

#include "Cards.h"
#include "Blackjack.h"
#include "io.h"

using namespace std;

namespace {
  const int cardvals[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10};
}

int
BlackjackCard::value (void) {
  return cardvals[va];
}

void
Hand::deal (void) {
  add(shoe.draw());
  add(shoe.draw());
}

void
Hand::add (Card *c) {
  cards.push_back(c);
}

void
Hand::muck (void) {
  cards.clear();
}

int
Hand::hit (void) {
  Card *card = shoe.draw();
  cout << card->name() << endl;
  add(card);
  showvalue(true);
  if (busted()) {
    cout << "[BUST]" << endl;
    return 0;
  }
  return value();
}

bool
Hand::busted (void) {
  return (value() == 0);
}

bool
Hand::blackjack (void) {
  return ((value() == 21) && (cards.size() == 2));
}

namespace {
  bool gt21 (int n) {return n > 21;} // my kingdom for a lambda operator
}

int
Hand::value (void) {
  vector<int> vals = values();

  vector<int>::iterator r = remove_if(vals.begin(), vals.end(), gt21);
  vals.resize(r - vals.begin());

  if (vals.size() == 0)
    return 0;
  else
    return *max_element(vals.begin(), vals.end());
}


vector<int>
Hand::values() {
  int val = 0, aces = 0;

  for (vector<Card *>::iterator c = cards.begin(); c != cards.end(); ++c) {
    int v = (*c)->value();
    if (v == 1)
      ++aces;
    else
      val += v;
   }

  vector<int> vals;
  vals.push_back(val);

  for (int i = 0; i < aces; ++i) {
    // XXX there is almost certainly a better way
    vector<int> newvals;
    for (vector<int>::iterator i = vals.begin(); i != vals.end(); ++i) {
      newvals.push_back(*i + 1);
      newvals.push_back(*i + 11);
    }
    vector<int>::iterator u = unique(newvals.begin(), newvals.end());
    newvals.resize(u - newvals.begin());
    vals = newvals;
  }
  
  return vals;
}

void
Hand::show (bool reveal) {
  if (cards.size() == 0) {
    cout << "[no cards]" << endl;
    return;
  }

  if (reveal)
    cout << " " << cards.front()->name() << endl;
  else
    cout << " one face down card" << endl;

  for (vector<Card *>::iterator c = ++cards.begin(); c != cards.end(); ++c)
    cout << " " <<  (*c)->name() << endl;

  showvalue(reveal);
}
    
int
PlayerHand::hit (void) {
  cout << "You draw the ";
  return Hand::hit();
}

void
Hand::showvalue (bool reveal) {
  if (reveal) {
    vector<int> vals = values();

    cout << "Total value: " << vals.front();
    for (vector<int>::iterator i = ++vals.begin(); i != vals.end(); ++i) {
      cout << " / " << *i;
    }
    cout << endl;
  } else {
    cout << "Total value: ???" << endl;
  }
}

void
PlayerHand::show (bool reveal) {
  cout << "Player has:" << endl;
  Hand::show(true);
}

int
PlayerHand::play (void) {
  bool first_draw = true;
  char action;
  double newbet;

  // XXX - insurance
  if (blackjack()) {
    cout << "[BLACKJACK]" << endl;
    return 21;
  }
    
  while (1) {	// actually, until we bust, surrender or stand
    // XXX - split
    // XXX - this is `late surrender', early surrender has to be
    //       handled at insurance time, if it is to be offered
    if (first_draw)
      action = getresp("[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
			  "Please enter [H], [D], [S], or [U]: ",
			  "hdsu", 0);
    else
      action = getresp("[H]it or [S]tand (HS)? ",
			  "Please enter [H] or [S]: ",
			  "hs", 0);

    switch (action) {
    case 'h':
      if (hit() == 0)
	return 0;
      else
	  // XXX some casinos allow DD after split.  some don't (confirm)
	first_draw = false;
      break;
    case 's':
      cout << "You stand" << endl;
      return value();
      break;
    case 'd':
      if (purse.getPurse() < purse.getMin()) {
	cout << "You cannot afford to double down!" << endl;
	continue;
      }
      newbet = getbet(purse.getMin(), min(purse.getBet(), purse.getLimit()));
      purse.doubledown(newbet);
      return hit();
      break;
    case 'u':
      cout << "You surrender" << endl;
      purse.surrender();
      return 0;
      break;
    }
  }
}

int
DealerHand::hit (void) {
  cout << "Dealer draws the ";
  return Hand::hit();
}

void
DealerHand::show (bool reveal) {
  cout << "Dealer has:" << endl;
  Hand::show(reveal);
}

int
DealerHand::play (void) {
  if (blackjack()) {
    cout << "[BLACKJACK]" << endl;
    return 21;
  }

  cout << "The dealer reveals the " << cards.front()->name() << endl;
  showvalue(true);

  while (1) {
    // XXX XXX should dealer hit a soft 17?  should this be configurable?
    if (value() < 17) {
      if (hit() == 0)
	return 0;
    } else {
      cout << "Dealer stands" << endl;
      return value();
    }
  }

}
