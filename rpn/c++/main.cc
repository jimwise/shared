#include <cstdlib>
#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <vector>

#include "ops.hh"
#include "calc.hh"
#include "rpn.hh"

int main (int argc, char **argv) {
  RPNCalc c;
  string s;
  cout << "> ";
  while (cin >> s) {
    c.act(s);
    cout << "> ";
  }
  cout << endl;
}
