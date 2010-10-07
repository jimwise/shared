(load "utils.ss")
(load "cards.ss")
(load "io.ss")
(load "blackjack.ss")

(import (chezscheme)
        (draga io)
        (draga cards)
        (draga blackjack))

(define table-min 5.00)
(define table-limit 1000.00)

(define player-purse 1000.00)

(define (deal! player-hand dealer-hand)
  (add-card! player-hand (draw!))
  (add-card! player-hand (draw!))
  (add-card! dealer-hand (draw!))
  (add-card! dealer-hand (draw!))
  (show-hands player-hand dealer-hand #f))

(define (show-hands player-hand dealer-hand reveal)
  (display "Dealer has:\n")
  (show-hand dealer-hand reveal)
  (display "\n")
  (display "Player has:\n")
  (show-hand player-hand #t))

(define getbet)
(let ([player-last-bet table-min])
  (set! getbet (lambda ()
                 (let ([n (getnum table-min (min table-limit player-purse) table-min player-last-bet
                                  (format "Please enter a bet (min = $~8,2f, limit = $~8,2f) [~8,2f]: "
                                          table-min table-limit player-last-bet))])
                   (set! player-last-bet n)
                   n))))

(define (playerplays! player-hand)
;; XXX - insurance
  (if (blackjack? player-hand)
      (begin (display "[BLACKJACK]\n") 21)
      (let ([getnext (lambda ()
                       (getresp "[H]it or [S]tand (HS)? "
                                "Please enter [H] or [S]: "
                                '(("h" hit) ("s" stand)) ""))])
        (let next
            ;; XXX - split
            ([action (getresp
                      "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? "
                      "Please enter [H], [D], [S], or [U]: "
                      '(("h" hit) ("d" double) ("s" stand) ("u" surrender))
                      "")])
          (cond
           [(symbol=? action 'hit)
            (display "You draw the ")
            (if (= (hit! player-hand) 0) 0 (next (getnext)))]
           [(symbol=? action 'stand)
            (display "You stand\n")
            (hand-value player-hand)]
           [(symbol=? action 'double)
            (if (< player-purse table_min)
                (begin
                  (display "You cannot afford to double down!\n")
                  (next (getnext)))
                (begin
                  (hand-bet-set! player-hand (+ (hand-bet player-hand) (getbet)))
                  (set! player-purse (- player-purse (hand-bet player-hand)))
                  (display "You draw the ")
                  (if (= (hit! player-hand) 0) 0 (next (getnext)))))]
            ;; XXX - this is `late surrender', early surrender has to be
            ;;       handled at insurance time, if it is to be offered
           [(symbol=? action 'surrender)
            (display "You surrender\n")
            (set! player-purse (- player_purse (* 0.5 (hand-bet player-hand))))
            0])))))

(define (dealerplays! dealer-hand)
  (if (blackjack? dealer-hand)
      (begin (display "[BLACKJACK]\n") 21)
      (begin
        (display (format "The dealer reveals the ~a\n" (card-name (car (hand-cards dealer-hand)))))
        (let next ()
          ;; XXX XXX should dealer hit a soft 17?  should this be configurable?
          (if (< (hand-value dealer-hand) 17)
              (begin
                (display "The dealer draws the ")
                (if (= (hit! dealer-hand) 0) 0 (next)))
              (begin
                (display "Dealer stands\n")
                (hand-value dealer-hand)))))))

(define (play-one-hand!)
  (let ([player-hand (make-hand '() 0.00)]
        [dealer-hand (make-hand '() 0.00)])
    (hand-bet-set! player-hand (getbet))
    (set! player-last-bet (hand-bet player-hand))
    (set! player-purse (- player-purse (hand-bet player-hand)))

    (deal! player-hand dealer-hand)

    (let ([players-best (playerplays! player-hand)])
      (cond
       [(busted? player-hand) (display "Dealer wins\n")]
       [(blackjack? player-hand)
        (display "Player wins\n")
        (set! player-purse (+ player-purse (* 2.5 (hand-bet player-hand))))]
       [else
        (newline)
        (let ([dealers-best (dealerplays! dealer-hand)])
          (if (= dealers-best 0)
              (begin
                (display "Player wins\n")
                (set! player-purse (+ player-purse (* 2 (hand-bet player-hand)))))
              (begin
                (newline)
                (show-hands player-hand dealer-hand #t)
                (newline)
                (cond
                 [(> dealers-best players-best)
                  (display "Player wins\n")
                  (set! player-purse (+ player-purse (* 2 (hand-bet player-hand))))]
                 [(< dealers-best players-best) (display "Dealer wins\n")]
                 [else
                  (display "Push")
                  (set! player-purse (+ player-purse (hand-bet player-hand)))]))))]))))

;; main routine

(display (format "You have: ~8,2f\n" player-purse))
(let next ()
    (play-one-hand!)

    (if (< player-purse table-min)
        (display "You're out of money!\n")
        (begin
          (display (format "You have: ~8,2f\n" player-purse))
          (when (symbol=? (getresp "Continue ([Y]es or [N]) ([Y]N)? "
                              "Please answer [Y]es or [N]o (default Y): "
                              '(("y" yes) ("n" no)) "y")
                     'yes)
            (next)))))
