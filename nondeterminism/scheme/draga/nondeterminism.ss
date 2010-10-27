(library (draga nondeterminism)
         (export reset-nd! choose fail mark cut)
         (import (rnrs))
                           
(define *paths* '())
(define failsym '__fail__)

(define (reset-nd!) (set! *paths* '()))

(define (choose choices)
  (if (null? choices)
      (_fail)
      (call-with-current-continuation
       (lambda (cc)
         (set! *paths*
               (cons (lambda ()
                       (cc (choose (cdr choices))))
                     *paths*))
         (car choices)))))

;; we wrap fail around a _fail procedure which is actually set! since an
;; exported variable cannot be set! in R6RS
(define _fail)
(define (fail) (_fail))

(define (mark) (set! *paths* (cons _fail *paths*)))
(define (cut)
  (cond ((null? *paths*))
        ((equal? (car *paths*) _fail)
         (set! *paths* (cdr *paths*)))
        (else
         (set! *paths* (cdr *paths*))
         (cut))))

(call-with-current-continuation
 (lambda (cc)
   (set! _fail
         (lambda ()
           (if (null? *paths*)
               (cc failsym)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))
)
