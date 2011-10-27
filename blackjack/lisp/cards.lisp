(defpackage :cards
  (:use :common-lisp)
  (:export :draw :card-name :card-val :card-ord))

(in-package :cards)

(defconstant *decksinshoe* 6)
(defconstant +suits+ '(hearts diamonds clubs spades))
(defconstant +cards+ '(ace two three four five six seven eight nine ten jack queen king))

(defstruct card val suit)

(defun card-ord (c)
  (1+ (position (card-val c) +cards+)))

(defun card-name (c)
  (format nil "~a of ~a" (card-val c) (card-suit c)))

(defconstant +one-deck+
  (loop for s in +suits+ appending
       (loop for v in +cards+ collecting (make-card :val v :suit s))))

(defun make-shoe ()
  (nshuffle
   (do ((i 1 (+ i 1))
	(s '() (append s +one-deck+)))
       ((= i *decksinshoe*) s))))

(defvar shoe ())
(defun shuffle ()
  (format t "Refilling shoe with ~a decks~%" *decksinshoe*)
  (setf shoe (make-shoe)))
     
(defun draw ()
  (when (null shoe) (shuffle))
  (pop shoe))

;; Knuth Shuffle
;; from Rosetta Code:
;; http://rosettacode.org/wiki/Knuth_shuffle

(defun nshuffle (sequence)
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))

(defvar rs (make-random-state t))

(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))
 
(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
     do (rotatef (aref array (random i rs))
		 (aref array (1- i)))
     finally (return array)))
