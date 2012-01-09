#include <cmath>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>

using namespace std;

double
getbet(double min, double limit) {
  /* XXX check actual min/table_limit rules */
  double bet = 0.0L;
  static double last_bet = 5.00;
  string s;

  cout << fixed << setprecision(2);
  cout << "Please enter a bet (min = $" << min << ", limit = $" << limit << ") [$" << last_bet << "]: ";

  
  while (1) {
    getline(cin, s);
    if (s == "")
      return last_bet;

    istringstream ss(s);
    ss >> skipws;
    if (ss.peek() == '$')
      ss.ignore();
    ss >> bet;

    if (cin.fail()) {
      cout << "Bet must be a number of dollars, try again: ";
      continue;
    }

    if (bet < min) {
      cout << "Bet must be at least $" << min << ", try again: ";
      continue;
    }

    if (fmod(bet, min) != 0.0) {
      cout << "Bet must be in an increment of $" << min << ", try again: ";
      continue;
    }

    if (bet > limit) {
      cout << "Bet must be less than or equal to $" << limit << ", try again: ";
      continue;
    }

    /* if we get here, bet is good */
    last_bet = bet;
    return bet;
  }
}

char
getresp(string ps1, string ps2, string allowed, char def) {
  string s;
  cout << ps1;
  while (1) {
    getline(cin, s);

    if (s.length() == 0) {
      if (def)
	return def;
      else
	continue;
    }

    char c = tolower(s[0]);

    if (allowed.find(c) != string::npos)
	return c;

    cout << ps2;
  }
}
