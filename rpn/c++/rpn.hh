#include <iostream>
#include <map>
#include <sstream>
#include <string>

#include "ops.hh"
#include "calc.hh"

#ifndef RPN_H
#define RPN_H

using namespace std;

class ShowOp : public UnaryOp {
public:
  ShowOp(void) : UnaryOp("display the top value on the stack") {}
  virtual void action (double x, Stack &s) {
    cout << x << endl;
    s.push(x);
  }
};
class AddOp : public BinaryOp {
public:
  AddOp(void) : BinaryOp("replace the top two values on the stack with their sum") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x + y);
  }
};
class SubOp : public BinaryOp {
public:
  SubOp(void) : BinaryOp("replace the top two values on the stack with their difference") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x - y);
  }
}; 
class MulOp : public BinaryOp {
public:
  MulOp(void) : BinaryOp("replace the top two values on the stack with their product") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x * y);
  }
};
class DivOp : public BinaryOp {
public:
  DivOp(void) : BinaryOp("replace the top two values on the stack with their quotient") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x / y);
  }
};
class ExptOp : public BinaryOp {
public:
  ExptOp(void) : BinaryOp("replace the top two values on the stack, x and y,  with x^y") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(pow(x,y));
  }
};
class DropOp : public UnaryOp {
public:
  DropOp(void) : UnaryOp("remove the top value from the stack") {}
  virtual void action (double x, Stack &s) {}
};
class DupOp : public UnaryOp {
public:
  DupOp(void) : UnaryOp("duplicate the top value on the stack") {}
  virtual void action (double x, Stack &s) {
    s.push(x);
    s.push(x);
  }
};
class SwapOp : public BinaryOp {
public:
  SwapOp(void) : BinaryOp("swap the top two values on the stack") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x);
    s.push(y);
  }
};
class HelpOp : public NullaryOp {
public:
  HelpOp(Dict *d) : NullaryOp("show this help") {ops = d;}
  virtual void action (Stack &s) {
    cout << ops->size() << " Commands:" << endl;
    for (Dict::iterator i=ops->begin(); i!=ops->end(); ++i) {
      cout << i->first << " -- " << i->second->doc() << endl;
    }
  }
private:
  Dict *ops;
};


class RPNCalc : public Calc {
public:
  RPNCalc (void) {
    ops["+"] = new AddOp();
    ops["-"] = new SubOp();
    ops["*"] = new MulOp();
    ops["/"] = new DivOp();
    ops["^"] = new ExptOp();

    ops["."] = new ShowOp();
    ops["drop"] = new DropOp();
    ops["dup"] = new DupOp();
    ops["swap"] = new SwapOp();

    ops["help"] = new HelpOp(&ops);
  }
};
#endif
