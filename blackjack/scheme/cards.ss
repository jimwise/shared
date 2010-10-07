(library (draga cards)
         (export shuffle! draw! suits cards card-suit card-val card-name make-shoe)
         (import (chezscheme) (draga utils))
         
  (define decksinshoe 6)

  (define suits '(hearts diamonds clubs spades))
  (define cards '(ace two three four five six seven eight nine ten jack queen king))

  (define (make-card val suit)
    (cons val suit))

  (define (card-val c) (car c))
  (define (card-suit c) (cdr c))

  (define (card-name c) (format "~a of ~a" (card-val c) (card-suit c)))

  (define one-deck
    (fold-left append '()
               (map (lambda (s)
                      (map (lambda (v) (make-card v s)) cards))
                    suits)))

  (define (make-shoe)
    (knuth-shuffle
     (do ([i 1 (+ i 1)]
          [s '() (append s one-deck)])
         ((= i decksinshoe) s))))

  (define shoe '())

  (define (shuffle!)
    (display (format "Refilling shoe with ~a decks\n" decksinshoe))
    (set! shoe (make-shoe)))

  (define (draw!)
    (when (null? shoe) (shuffle!))
    (let ([c (car shoe)])
      (set! shoe (cdr shoe))
      c))
)
