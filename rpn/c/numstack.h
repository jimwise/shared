#ifndef NUMSTACK_H
#define NUMSTACK_H 1

#include <stddef.h>

typedef double num;

typedef struct stack {
  size_t sz;
  size_t bufsz;
  num *buf;
} *stack;

stack make_stack (void);
void push(stack, num);
num pop(stack);
size_t size(stack);
void free_stack (stack);

#endif
