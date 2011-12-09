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

;; op data structure
;; an op is a list of (str num str func)
;; which are (name arity doc code)
(define (make-action name arity doc action) (list name arity doc action))
(define (op-name op) (car op))
(define (op-arity op) (cadr op))
(define (op-doc op) (caddr op))
(define (op-action op) (cadddr op))

;; action : line stack -> stack
;; given an input line and the current stack, take the action requested and return
;; the updated stack
(define (action line stack)
  (cond
   [(eof-object? line) (newline) (exit)]
   [(string->number line) => (lambda (n) (cons n stack))]
   [(asss/ci line action-list) => (lambda (a)
                                    (if (>= (length stack) (op-arity a))
                                        ((op-action a) stack)
                                        (begin
                                          (signal-error "stack underflow")
                                          stack)))]
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
  (list (make-action "." 1 "" rpn-top)
        (make-action "+" 2 "" rpn-plus)
        (make-action "-" 2 "" rpn-minus)
        (make-action "*" 2 "" rpn-times)
        (make-action "/" 2 "" rpn-divby)
        (make-action "^" 2 "" rpn-expt)
        (make-action "drop" 1 "" rpn-drop)
        (make-action "dup" 1 "" rpn-dup)
        (make-action "swap" 2 "" rpn-swap)))

(define (rpn)
  (display "> ")
  (let ([in (current-input-port)])
    (let loop ([l (get-line in)]
               [s '()])
      (let ([new-s (action l s)])
        (display "> ")
        (loop (get-line in) new-s)))))
  
(rpn)
