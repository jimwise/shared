(library (draga utils)
         (export makeset string-join index/1 knuth-shuffle asss/ci)
         (import (chezscheme))
         
;; tls, chap 7 (but with memv instead of member?)
(define (makeset lat)
                  (cond
                   ((null? lat) (quote ()))
                   ((memv (car lat) (cdr lat)) (makeset (cdr lat)))
                   (else (cons (car lat) (makeset (cdr lat))))))

;; join : string list-of-strings -> string
;; return a list of strings made by concatenating a list of strings with
;; sep as separator
(define (string-join sep ls)
  (cond
   [(null? ls) ""]
   [(null? (cdr ls)) (car ls)]
   [else (string-append (car ls) sep (string-join sep (cdr ls)))]))

;; index/1 : atom list -> integer or boolean
;; return the base-one index of first instance of an item in a list, or #f
(define (index/1 i l)
  (cond
   [(null? l) #f]
   [(eq? i (car l)) 1]
   [else (+ 1 (index/1 i (cdr l)))]))

;; knuth-shuffle : list -> list
;; randomize the order of members of a list, using Knuth's Algorithm
;; implementation courtesy of Phil Bewing
;; -- http://list.cs.brown.edu/pipermail/plt-scheme/2009-August/035010.html
(define (knuth-shuffle x)
  (do ((v (list->vector x)) (n (length x) (- n 1)))
      ((zero? n) (vector->list v))
    (let* ((r (random n)) (t (vector-ref v r)))
      (vector-set! v r (vector-ref v (- n 1)))
      (vector-set! v (- n 1) t))))

;; asss : string alist -> alist or boolean
;; as assq, but uses string=? for comparisons
(define (asss str alist)
  (cond
   [(null? alist) #f]
   [(and (string? (caar alist)) (string=? str (caar alist))) (car alist)]
   [else (asss str (cdr alist))]))

;; asss/ci : string alist -> alist or boolean
;; as assq, but uses string-ci=? for comparisons
(define (asss/ci str alist)
  (cond
   [(null? alist) #f]
   [(and (string? (caar alist)) (string-ci=? str (caar alist))) (car alist)]
   [else (asss str (cdr alist))]))

;; from (draga utils)
;; .. : num num -> list-of-nums
;; given first and last, return a list of the range first .. last, inclusive
(define (.. first last)
   (if (= first last)
       (list first)
       (cons first (.. (+ first 1) last))))

; intialization
(random-seed (time-second (current-time)))
)
