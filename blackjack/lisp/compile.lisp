(load "black.lisp")
(sb-ext:disable-debugger)
(sb-ext:save-lisp-and-die "black"  :toplevel #'blackjack :executable t :purify t)
