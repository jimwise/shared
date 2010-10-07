(library (draga blackjack)
(export add-card!  blackjack?  busted?  hand-bet hand-bet-set!  hand-cards
        hand-cards-set!  hand-value hit!  make-hand show-hand)
(import (draga utils) (draga cards) (chezscheme))

(define (card-values c)
  (let ([v (index/1 (card-val c) cards)])
    (cond
     [(= v 1) (list 1 11)]
     [(> v 10) 10]
     [else v])))

(define-record-type hand
  (fields (mutable cards)
          (mutable bet)))

(define (hand-values hand)
  (letrec ([cvals (map card-values (hand-cards hand))]
           [vs (lambda (cards vals)
                 (cond
                  [(null? cards) vals]
                  [(atom? (car cards))
                   (vs (cdr cards) (map (lambda (n) (+ n (car cards))) vals))]
                  [else (vs (cdr cards)
                            (makeset
                             (fold-left append '() (map (lambda (n)
                                                          (list
                                                           (+ (caar cards) n)
                                                           (+ (cadar cards) n))) vals))))]))])
    (vs cvals (list 0))))

(define (hand-value hand)
  (apply max (filter (lambda (n) (< n 22)) (cons 0 (hand-values hand)))))

(define (busted? hand) (= (hand-value hand) 0))

(define (blackjack? hand) (and (= (length (hand-cards hand)) 2) (= (hand-value hand) 21)))

(define (show-hand hand reveal)
  (let ([cards (hand-cards hand)]
        [show (lambda (c) (display (format "  ~a\n" (card-name c))))])
    (if (null? cards)
        (display "no cards\n")
        (begin
          (if reveal
              (show (car cards))
              (display "  one face down card\n"))
          (for-each show (cdr cards))))
    (show-value hand reveal)
    (when (> (hand-bet hand) 0) (display (format "Bet: $~8,2F\n" (hand-bet hand))))))

(define (show-value hand reveal)
  (if reveal
      (display (format "Total value: ~a\n"
                       (string-join " / " (map number->string (hand-values hand)))))
      (display "Total value: ???\n")))

(define (add-card! hand card)
  (hand-cards-set! hand (reverse (cons card (hand-cards hand)))))

(define (hit! hand)
  (let ([c (draw!)])
    (display (format "~a\n" (card-name c)))
    (add-card! hand c)
    (show-value hand #t)
    (when (busted? hand) (display "[BUST]\n"))
    (hand-value hand)))

;; for testing
;; (define h1 (make-hand '((two . clubs) (ten . hearts)) 15))
;; (define h2 (make-hand '((two . clubs) (ace . hearts)) 15))
;; (define h3 (make-hand '((two . clubs) (ace . hearts) (three . spades) (ace . spades)) 15))
;; (define h4 (make-hand '((jack . clubs) (ace . hearts)) 15))

)
