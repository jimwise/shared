#include <setjmp.h>
#include <stdio.h>
#include "nondeterminism.h"

#define MAX_DEPTH 100		/* could move to linked list later */
#define CUT_MARKER -1

struct nd_path nd_paths[MAX_DEPTH];
int nd_currpath = -1;
int nd_debug = 0;

void
nd_reset (void) {
  nd_currpath = -1;
}

void
fail (void) {
  if (nd_debug)
    printf("FAIL: nd_currpath = %d\n", nd_currpath);
  while (nd_paths[nd_currpath].t == CUT_MARKER)
    nd_currpath--;
  if (nd_currpath > -1)
    longjmp(nd_paths[nd_currpath].j, -1);
  else {
    printf("FAIL\n");
    exit(0);
  }
}
  
void
mark (void) {
  nd_paths[++nd_currpath].t = CUT_MARKER;
}

void
cut (void) {
  while ((nd_currpath > -1) && (nd_paths[nd_currpath].t != CUT_MARKER))
    nd_currpath--;
  /* nd_paths has now been unwound back to before last _cut_marker */
}

