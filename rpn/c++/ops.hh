#include <iostream>
#include <map>
#include <stack>
#include <sstream>
#include <string>

#ifndef OPS_H
#define OPS_H

using namespace std;

void signal_error (string str) {cout << "ERROR: " << str << endl;}

typedef stack<double> Stack;

class Op {
public:
  Op(string d) {_doc = d;}
  virtual void act(Stack &s) = 0;
  string &doc (void) {return _doc;}
private:
  string _doc;
};

class NullaryOp : public Op {
public:
  NullaryOp(string d) : Op(d) {};
  virtual void act(Stack &s) {
      action(s);
  }
protected:
  virtual void action(Stack &s) = 0;
};

class UnaryOp : public Op {
public:
  UnaryOp(string d) : Op(d) {};
  virtual void act(Stack &s) {
    if (s.size() < 1) {
      signal_error("stack underflow");
    } else {
      double x = s.top(); s.pop();
      action(x, s);
    }
  }
protected:
  virtual void action(double x, Stack &s) = 0;
};

class BinaryOp : public Op {
public:
  BinaryOp(string d) : Op(d) {};
  virtual void act(Stack &s) {
    if (s.size() < 2) {
      signal_error("stack underflow");
    } else {
      double y = s.top(); s.pop();
      double x = s.top(); s.pop();
      action(x, y, s);
    }
  }
protected:
  virtual void action(double x, double y, Stack &s) = 0;
};

#endif
