#ifndef NONDETERMINISM_H
#define NONDETERMINISM_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <setjmp.h>

#define MAX_DEPTH 100

int _currpath = -1;
jmp_buf _paths[MAX_DEPTH];

  /* chcs is int[n], x gets choice */
#define CHOOSE(x, n, chcs)			\
  {						\
    int _chc = n - 1;				\
    setjmp(_paths[++_currpath]);		\
    if (_chc == -1) {				\
      _currpath--;				\
      FAIL();					\
    } else {					\
      x = chcs[_chc--];				\
    }						\
  }
  
#define FAIL() longjmp(_paths[_currpath], -1)

#define ND_INIT  setjmp(_paths[++_currpath])

#ifdef  __cplusplus
}
#endif

#endif
