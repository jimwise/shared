#include <stdio.h>
#include <stdlib.h>
#include "numstack.h"

const size_t BUFSZ = 128;

stack
make_stack (void) {
  stack s = malloc(sizeof(struct stack));
  s->sz = 0;
  s->buf = malloc(BUFSZ * sizeof(num));
  s->bufsz = BUFSZ;
  return s;
}

void
push(stack s, num n) {
  if (s->sz == s->bufsz) {
    s->bufsz *= 2;
    if ((s->buf = realloc(s->buf, s->bufsz * sizeof(num))) == NULL) {
      fputs("*** Stack resize failed\n", stderr);
      exit(1);
    }
  }
  s->sz++;
  s->buf[s->sz - 1] = n;
}

num
pop(stack s) {
  if (s->sz == 0) {
    fputs("*** Stack underflow\n", stderr);
    exit(1);
  }
  return s->buf[--s->sz];
}

size_t
size(stack s) {
  return s->sz;
}

void
free_stack (stack s) {
  free(s->buf);
  free(s);
}
