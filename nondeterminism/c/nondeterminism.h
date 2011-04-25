#ifndef NONDETERMINISM_H
#define NONDETERMINISM_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <setjmp.h>

  struct nd_path {
    int t;
    jmp_buf j;
  };

  extern struct nd_path nd_paths[];
  extern int nd_currpath;
  extern int nd_debug;

  void nd_reset (void);

  /* Must be a macro, as we cannot longjmp into a function we've returned from  */
  /* chcs is T[n], x gets choice */
#define choose(x, n, chcs)						\
  {									\
    int nd_chc = n - 1;							\
    nd_currpath++;							\
    nd_paths[nd_currpath].t = 0;					\
    setjmp(nd_paths[nd_currpath].j);					\
    if (nd_debug)							\
      printf("CHOOSE (%p): _chc = %d\n", &nd_chc, nd_chc);		\
    if (nd_chc == -1) {							\
      if (nd_debug)							\
	printf ("CHOOSE (%p): empty choices -- failing\n", &nd_chc);	\
      nd_currpath--;							\
      fail();								\
    }									\
    x = chcs[nd_chc--];							\
  }

  void fail (void);
  void mark (void);
  void cut (void);

#ifdef  __cplusplus
}
#endif

#endif
