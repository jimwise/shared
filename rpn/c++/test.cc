#include <cassert>
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
  c.act("2");
  c.act("2");
  c.act("+");
  assert(c.top() == 4.0);
  c.act("drop");
  assert(c.size() == 0);
}
