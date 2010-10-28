#include <setjmp.h>
#include <stdio.h>
#include "nondeterminism.h"

#define MAX_DEPTH 100		/* could move to linked list later */
#define CUT_MARKER -1

struct _path _paths[MAX_DEPTH];
int _currpath = -1;

void
nd_reset (void) {
  _currpath = -1;
}

void
fail (void) {
  /* printf("FAIL: _currpath = %d\n", _currpath); */
  while (_paths[_currpath].t == CUT_MARKER)
    _currpath--;
  if (_currpath > -1)
    longjmp(_paths[_currpath].j, -1);
}
  
void
mark (void) {
  _paths[++_currpath].t = CUT_MARKER;
}

void
cut (void) {
  while ((_currpath > -1) && (_paths[_currpath].t != CUT_MARKER))
    _currpath--;
  /* _paths has now been unwound back to before last _cut_marker */
}

