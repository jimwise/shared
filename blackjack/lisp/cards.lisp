(defparameter *decksinshoe* 6)
;; should be defconstant, but for REPL use
(defparameter +suits+ '(hearts diamonds clubs spades))
(defparameter +cards+ '(ace two three four five six seven eight nine ten jack queen king))

(defun make-card (val suit)
  (cons val suit))

(defparameter +one-deck+
  (loop for s in +suits+ appending
       (loop for v in +cards+ collecting (make-card v s))))

(defun card-val (c) (car c))
(defun card-suit (c) (cdr c))

;; hack? -- not sure if w-o-t-s is best way here
(defun card-name (c)
  (with-output-to-string (st) (format st "~a of ~a" (card-val c) (card-suit c))))

(defun make-shoe ()
  (nshuffle
   (do ((i 1 (+ i 1))
	(s '() (append s +one-deck+)))
       ((= i *decksinshoe*) s))))

(defparameter *shoe* '())

(defun shuffle ()
  (format t "Refilling shoe with ~a decks~%" *decksinshoe*)
  (setf *shoe* (make-shoe)))

(defun draw ()
  (when (null *shoe*) (shuffle))
  (pop *shoe*))

;; Knuth Shuffle
;; from Rosetta Code:
;; http://rosettacode.org/wiki/Knuth_shuffle

(defun nshuffle (sequence)
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))
 
(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))
 
(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))
