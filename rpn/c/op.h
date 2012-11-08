#ifndef OP_H
#define OP_H

#include <stddef.h>
#include "numstack.h"

typedef void (*action)(stack);

typedef struct {
  const char *name;
  const char *doc;
  const size_t arity;
  action act;
} op;

#define op_unknown {"", "", 0, NULL}

typedef op opdict[];

void signal_error (char *, ...);
void operate (stack, op);
op lookup (opdict, char *);
int op_found (op);

#endif
