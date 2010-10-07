(library (draga io)
         (export getresp getnum)
         (import (chezscheme) (draga utils))

(define (read-line/default default)
  (let ([l (get-line (current-input-port))])
    (if (string=? l "") default l)))
  
;; getresp : string string list-of-pair symbol
;; allowed is an alist of ( string symbol )
;; prompt with prompt1, and get a response character, returning the matching symbol
;; from allowed, or `default' if user hits <return>.
;; if user enters a disallowed character, loop, prompting with prompt2

(define (getresp prompt1 prompt2 allowed default)
  (display prompt1)
  (let retry ([c (asss/ci (read-line/default default) allowed)])
    (if c
        (cadr c)
        (begin (display prompt2) (retry (asss/ci (read-line/default default) allowed))))))

; example
; (display (getresp "foo (y/n): " "bar (y/n): " '(("y" 'yes) ("n" 'no)) 'yes))

;; getnum : num num num num string -> num
;; prompts for a number between min and max, integer divisible by step, and keeps trying until
;; it gets one.  if user hits return, use default
(define (getnum min max step default prompt)
  (display prompt)
  (let ([in (current-input-port)])
    (let retry
        ([l (get-line in)])
      (if (string=? l "")
          default
          (let ([n (string->number l)])
            (cond
             [(not n)
              (display "Amount must be a number, try again: ")
              (retry (get-line in))]
             [(< n min)
              (display (format "Amount must be at least ~8,2F, try again: " min))
              (retry (get-line in))]
             [(> n max)
              (display (format "Amount must be at most ~8,2F, try again: " max))
              (retry (get-line in))]
             [(not (= 0 (mod n step)))
              (display (format "Amount must be in increments of ~8,2F, try again: " step))
              (retry (get-line in))]
             [else n]))))))

;example
;(display (getnum 5 1000 5 10 "foobar: "))
)
