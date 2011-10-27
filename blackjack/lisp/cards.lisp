(defparameter *decksinshoe* 6)
;; should be defconstant, but for REPL use
(defparameter +suits+ '(hearts diamonds clubs spades))
(defparameter +cards+ '(ace two three four five six seven eight nine ten jack queen king))

(defstruct card val suit)

(defparameter +one-deck+
  (loop for s in +suits+ appending
       (loop for v in +cards+ collecting (make-card :val v :suit s))))

(defun card-name (c)
  (format nil "~a of ~a" (card-val c) (card-suit c)))

(defun make-shoe ()
  (nshuffle
   (do ((i 1 (+ i 1))
	(s '() (append s +one-deck+)))
       ((= i *decksinshoe*) s))))


(let ((shoe ()))
  (defun shuffle ()
    (format t "Refilling shoe with ~a decks~%" *decksinshoe*)
    (setf shoe (make-shoe)))
     
  (defun draw ()
    (when (null shoe) (shuffle))
    (pop shoe)))

;; Knuth Shuffle
;; from Rosetta Code:
;; http://rosettacode.org/wiki/Knuth_shuffle

(defun nshuffle (sequence)
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))

(let ((rs (make-random-state t))) 
  (defun nshuffle-list (list)
    "Shuffle the list using an intermediate vector."
    (let ((array (nshuffle-array (coerce list 'vector))))
      (declare (dynamic-extent array))
      (map-into list 'identity array)))
 
  (defun nshuffle-array (array)
    (loop for i from (length array) downto 2
       do (rotatef (aref array (random i rs))
		   (aref array (1- i)))
       finally (return array))))
