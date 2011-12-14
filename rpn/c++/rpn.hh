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
  SwapOp(void) : BinaryOp("duplicate the top value on the stack") {}
  virtual void action (double x, double y, Stack &s) {
    s.push(x);
    s.push(y);
  }
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
  }
};
