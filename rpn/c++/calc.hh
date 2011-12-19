#include <iostream>
#include <map>
#include <stack>
#include <sstream>
#include <string>

#include "ops.hh"

#ifndef CALC_H
#define CALC_H

using namespace std;

typedef map<string, Op *> Dict;

class Calc {
public:
  void act(string str) {
    Dict::iterator opi = ops.find(str);
    if (opi != ops.end()) {
      opi->second->act(s);
    } else {
      istringstream i(str);
      double d;
      if (!(i >> d)) {
	signal_error("unknown operation");
      } else {
	s.push(d);
      }
    }
  }
  // used in test routines
  double top(void) {return s.top();}
  unsigned int size(void) {return s.size();}
protected:
  Stack s;
  Dict ops;
};

#endif
