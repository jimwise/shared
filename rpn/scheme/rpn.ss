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


;; action : line stack -> stack
;; given an input line and the current stack, take the action requested and return
;; the updated stack
(define (action line stack)
  (cond
   [(eof-object? line) (newline) (exit)]
   [(string->number line) => (lambda (n) (cons n stack))]
   [(asss/ci line action-list) => (lambda (a) ((cdr a) stack))]
   [else (display "ERROR\n") (exit #f)]))

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
  `(("." . ,rpn-top)
    ("+" . ,rpn-plus)
    ("-" . ,rpn-minus)
    ("*" . ,rpn-times)
    ("/" . ,rpn-divby)
    ("^" . ,rpn-expt)
    ("drop" . ,rpn-drop)
    ("dup" . ,rpn-dup)
    ("swap" . ,rpn-swap)))

(define (rpn)
  (display "> ")
  (let ([in (current-input-port)])
    (let loop ([l (get-line in)]
               [s '()])
      (let ([new-s (action l s)])
        (display "> ")
        (loop (get-line in) new-s)))))
  
(rpn)
