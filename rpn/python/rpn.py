#!/usr/bin/python

from __future__ import print_function

import sys

ops = {}
docs = {}

def signalError(string):
    print("*** ERROR: " + string)

# given an arity, return a decorator for that arity
def operation(name, arity, doc):
    def decorate(op):
        def decorated(stack):
            args = []
            if len(stack) < arity:
                signalError("stack underflow")
                return stack
            else:
                for i in range(arity):
                    args.append(stack.pop())
                args.reverse()
                op(stack, *args)
        ops[name] = decorated
        docs[name] = doc
        return decorated
    return decorate

@operation ('.', 1, 'display the top value on the stack')
def op_show (stack, x):
    print(x)
    stack.append(x)

@operation('#', 0, 'display number of values on the stack')
def op_size (stack):
    print(len(stack))

@operation('+', 2, 'replace the top two values on the stack with their sum')
def op_plus (stack, x, y):
    stack.append(x + y)

@operation('-', 2, 'replace the top two values on the stack with their difference')
def op_minus (stack, x, y):
    stack.append(x - y)

@operation('*', 2, 'replace the top two values on the stack with their product')
def op_times (stack, x, y):
    stack.append(x * y)

@operation('/', 2, 'replace the top two values on the stack with their quotient')
def op_divby (stack, x, y):
    stack.append(x / y)

@operation('^', 2, 'replace the top two values on the stack, x and y, with x to the yth power')
def op_expt (stack, x, y):
    stack.append(x ** y)

@operation('drop', 1, 'remove the top value from the stack')
def op_drop (stack, x, y):
    pass

@operation('dup', 1, 'duplicate the top value on the stack')
def op_dup (stack, x):
    stack.append(x)
    stack.append(x)

@operation('swap', 2, 'swap the top two values on the stack')
def op_swap (stack, x, y):
    stack.append(y)
    stack.append(x)

@operation ('help', 0, 'display this help')
def op_help (stack):
    print(str(len(docs.keys())) + " Commands:")
    for c in docs.keys():
        print(c + " -- " + docs[c])

stack = []

def action(string):
    if string in ops:
        ops[string](stack)
    else:
        try:
            stack.append(float(string))
        except ValueError:
            signalError("unknown operation")

while True:
    print("> ", end='')
    string = sys.stdin.readline()
    if string == '':
        break
    if string == '\n':
        continue
    action(string[:-1])

print('')
