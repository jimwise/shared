#ifndef NONDETERMINISM_H
#define NONDETERMINISM_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <setjmp.h>

  struct _path {
    int t;
    jmp_buf j;
  };

  extern struct _path _paths[];
  extern int _currpath;

  void nd_reset (void);

  /* Must be a macro, as we cannot longjmp into a function we've returned from  */
  /* chcs is T[n], x gets choice */
#define choose(x, n, chcs)			\
  {						\
    int _chc = n - 1;				\
    _currpath++;				\
    _paths[_currpath].t = 0;			\
    setjmp(_paths[_currpath].j);		\
    printf("CHOOSE: _chc = %d\n", _chc);	\
    if (_chc == -1) {				\
      _currpath--;				\
      fail();					\
    }						\
    x = chcs[_chc--];				\
  }

  void fail (void);
  void mark (void);
  void cut (void);

#ifdef  __cplusplus
}
#endif

#endif
