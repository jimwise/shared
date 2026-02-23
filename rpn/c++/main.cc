#include <cstdlib>
#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <vector>

#include "ops.hh"
#include "calc.hh"
#include "rpn.hh"

using namespace std;

int main(__attribute__((unused)) int argc, __attribute__((unused)) char **argv) {
  RPNCalc c;
  string s;
  cout << "> ";
  while (cin >> s) {
    c.act(s);
    cout << "> ";
  }
  cout << endl;
}
