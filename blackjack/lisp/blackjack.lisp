(load "cards.lisp")

(defpackage :blackjack
  (:use :common-lisp :cards)
  (:export :make-hand
	   :show-hand
	   :blackjackp
	   :hand-cards
	   :hand-value
	   :hand-add-card
	   :hand-bet
	   :hit))

(in-package :blackjack)

(defun card-values (c)
  (let ((v (card-ord c)))
    (cond
     ((= v 1) (list 1 11))
     ((> v 10) 10)
     (t v))))

(defstruct hand
  (cards '())
  (bet 0.00))

(defun hand-add-card (hand card)
  (setf (hand-cards hand) (append (hand-cards hand) (list card))))

(defun hand-values (hand)
  (let* ((cvals (mapcar #'card-values (hand-cards hand)))
	 (hvals (list 0)))
    (dolist (val cvals)
      (if (atom val)
	  (map-into hvals #'(lambda (n) (+ n val)) hvals)
	  ;; we know that a card only has one or two values.  this helps.
	  (setf hvals (union (mapcar #'(lambda (n)
					 (+ n (first val)))
				     hvals)
			     (mapcar #'(lambda (n)
					 (+ n (second val)))
				     hvals)))))
    hvals))

(defun hand-value (hand)
   (apply #'max (remove-if-not #'(lambda (n) (< n 22)) (cons 0 (hand-values hand)))))

(defun bustedp (hand) (zerop (hand-value hand)))

(defun blackjackp (hand) (and (= (length (hand-cards hand)) 2) (= (hand-value hand) 21)))

(defun show-hand (hand reveal)
  (let ((cards (hand-cards hand))
	       (show (lambda (c) (format t "  ~a~%" (card-name c)))))
    (if (null cards)
	(format t "no cards~%")
	(progn
	  (if reveal
	      (funcall show (car cards))
	      (format t "  one face down card~%"))
	  (mapc show (rest cards))))
    (show-value hand reveal)
    (when (> (hand-bet hand) 0) (format t "Bet: $~8,2F~%" (hand-bet hand)))))

(defun show-value (hand reveal)
  (if reveal
      (format t "Total value: ~{~a~^ / ~}~%" (hand-values hand))
      (format t "Total value: ???~%")))

(defun hit (hand)
  (let ((c (draw)))
    (format t "~a~%" (card-name c))
    (hand-add-card hand c)
    (show-value hand t)
    (when (bustedp hand) (format t "[BUST]~%"))
    (hand-value hand)))
