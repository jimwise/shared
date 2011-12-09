#!r6rs

;; two funcs from (draga utils)
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

;; signal-error : str -> void
;; given an error message, pass it to the user in a standardish way
(define (signal-error str)
  (display (format "*** ERROR: ~a~%" str)))

;; action : line stack -> stack
;; given an input line and the current stack, take the action requested and return
;; the updated stack
(define (action line stack)
  (cond
   [(eof-object? line) (newline) (exit)]
   [(string->number line) => (lambda (n) (cons n stack))]
   [(asss/ci line action-list) => (lambda (a) ((cadddr a) stack))]
   [else (signal-error "unknown operation") stack]))

(define (rpn-top stack)
  (display (car stack))
  (newline)
  stack)

(define (rpn-plus stack)
  (cons (+ (cadr stack) (car stack)) (cddr stack)))

(define (rpn-minus stack)
  (cons (- (cadr stack) (car stack)) (cddr stack)))

(define (rpn-times stack)
  (cons (* (cadr stack) (car stack)) (cddr stack)))

(define (rpn-divby stack)
  (cons (/ (cadr stack) (car stack)) (cddr stack)))

(define (rpn-expt stack)
  (cons (expt (car stack) (cadr stack)) (cddr stack)))

(define (rpn-drop stack)
  (cdr stack))

(define (rpn-dup stack)
  (cons (car stack) stack))

(define (rpn-swap stack)
  (cons (cadr stack) (cons (car stack) (cddr stack))))

(define action-list
  `(("." 1 "" ,rpn-top)
    ("+" 2 "" ,rpn-plus)
    ("-" 2 "" ,rpn-minus)
    ("*" 2 "" ,rpn-times)
    ("/" 2 "" ,rpn-divby)
    ("^" 2 "" ,rpn-expt)
    ("drop" 1 "" ,rpn-drop)
    ("dup" 1 "" ,rpn-dup)
    ("swap" 2 "" ,rpn-swap)))

(define (rpn)
  (display "> ")
  (let ([in (current-input-port)])
    (let loop ([l (get-line in)]
               [s '()])
      (let ([new-s (action l s)])
        (display "> ")
        (loop (get-line in) new-s)))))
  
(rpn)
