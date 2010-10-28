#ifndef NONDETERMINISM_H
#define NONDETERMINISM_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <setjmp.h>

#define ND_MAX_DEPTH 100

/*
 * WARNING:  Implementation specific:                                        
 *  we assume an uninitialized jmpbuf is != any initialized jmpbuf!
 */
int _currpath = -1;
jmp_buf _paths[ND_MAX_DEPTH];
jmp_buf _cut_marker;

#define ND_RESET() _currpath = -1; /* setjmp(_paths[0]); _currpath = 0; */

  /* chcs is T[n], x gets choice */
#define CHOOSE(x, n, chcs)			\
  {						\
    /* printf("CHOOSE\n"); */			\
    int _chc = n - 1;				\
    setjmp(_paths[++_currpath]);		\
    /* printf("_chc = %d\n", _chc); */		\
    if (_chc == -1) {				\
      --_currpath;				\
      FAIL();					\
    }						\
    x = chcs[_chc--];				\
  }
  
#define FAIL()						\
  {							\
    /* printf("FAIL: _currpath = %d\n", _currpath); */	\
    if (_paths[_currpath] == _cut_marker) {		\
      _currpath--;					\
    }							\
    if (_currpath > -1) {				\
      longjmp(_paths[_currpath], -1);			\
    }							\
  }

#define MARK() memcpy(_paths[++_currpath], _cut_marker, sizeof(jmp_buf));

#define CUT()								\
  {									\
    while ((_currpath > -1) && (_paths[_currpath--] != _cut_marker))	\
      ; /* do nothing */						\
    /* _paths has now been unwound back to before last _cut_marker */	\
  }

#ifdef  __cplusplus
}
#endif

#endif
