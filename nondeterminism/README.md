This directory contains three implementations of choose/fail nondeterministic
programming with branch cut, as described in Chapter 22 of Paul Graham's _On
Lisp_[1].

The directory `scheme' contains Paul Graham's scheme implementation, with a
few tweaks to bring it into an R6RS world.

The directory `c' contains choose/fail and the scheme examples demonstrating
its use reworked in c, using a preprocessor macro for choose, and setjmp/longjmp.

The directory `ruby' contains choose/fail and the scheme examples
demonstrating its use reworked in ruby.  This is a pretty straight port of
the scheme version, as ruby has true call/cc (Kernel#callcc).


  [1] Graham, Paul, _On Lisp_, Prentice Hall, 1993, ISBN 0130305529.
      Available online at

      	http://www.paulgraham.com/onlisp.html
