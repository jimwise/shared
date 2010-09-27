(load "blackjack.lisp")
(load "io.lisp")

(defparameter +table-min+ 5.00)
(defparameter +table-limit+ 1000.00)

(defparameter *player-purse* 1000.00)

(defun deal (player-hand dealer-hand)
  (hand-add-card player-hand (draw))
  (hand-add-card player-hand (draw))
  (hand-add-card dealer-hand (draw))
  (hand-add-card dealer-hand (draw))
  (show-hands player-hand dealer-hand nil))

(defun show-hands (player-hand dealer-hand reveal)
  (format t "Dealer has:~%")
  (show-hand dealer-hand reveal)
  (terpri)
  (format t "Player has:~%")
  (show-hand player-hand t))

(let ((player-last-bet +table-min+))
  (defun get-bet ()
    (let ((prompt
	   (format nil
		   "Please enter a bet (min = $~8,2f, limit = $~8,2f) [~8,2f]: "
		   +table-min+ +table-limit+ player-last-bet)))
      (setf player-last-bet (get-number prompt
					:min +table-min+
					:max (min +table-limit+ *player-purse*)
					:step +table-min+
					:default player-last-bet))
      player-last-bet)))

(defun player-play (player-hand)
  ;; XXX - insurance
  (if (blackjackp player-hand)
      (progn
	(format t "[BLACKJACK]~%")
	21)
      ;; XXX - split
      (loop for action = (get-response
			  "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? "
			  "Please enter [H], [D], [S], or [U]: "
			  '(("h" hit) ("d" double) ("s" stand) ("u" surrender)))
	 then (get-response "[H]it or [S]tand (HS)? "
			    "Please enter [H] or [S]: "
			    '(("h" hit) ("s" stand))) do
           (cond
	     ((eq action 'hit)
	      (format t "You draw the ")
	      (when (zerop (hit player-hand)) (return 0)))
	     ((eq action 'stand)
	      (format t "You stand~%")
	      (return (hand-value player-hand)))
	     ((eq action 'double)
	      (if (< *player-purse* +table-min+)
		  (format t "You cannot afford to double down!~%")
		  (progn
		    (hand-bet-set player-hand (+ (hand-bet player-hand) (get-bet)))
		    (decf *player-purse* (hand-bet player-hand))
		    (format t "You draw the ")
		    (when (zerop (hit player-hand)) (return 0)))))
            ;; XXX - this is `late surrender', early surrender has to be
            ;;       handled at insurance time, if it is to be offered
	     ((eq action 'surrender)
	      (format t "You surrender~%")
	      (decf *player-purse* (* 0.5 (hand-bet player-hand)))
	      (return 0))))))
	     
(defun dealer-play (dealer-hand)
  (if (blackjackp dealer-hand)
      (progn
	(format t "[BLACKJACK]~%")
	21)
      (progn
	(format t "The dealer reveals the ~a~%" (card-name (first (hand-cards dealer-hand))))
	(format t "The dealer has:~%")
	(show-hand dealer-hand t)
	;; XXX XXX should dealer hit a soft 17?  should this be configurable?
	(loop while (< (hand-value dealer-hand) 17) do
	       (format t "The dealer draws the ")
	       (when (zerop (hit dealer-hand)) (return-from dealer-play 0)))
	;; if we get here, we didn't bust
	(format t "Dealer stands~%")
	(hand-value dealer-hand))))

(defun play-one-hand ()
  (let ((player-hand (make-hand '() (get-bet)))
	(dealer-hand (make-hand)))

    (decf *player-purse* (hand-bet player-hand))

    (deal player-hand dealer-hand)

    (let ((players-best (player-play player-hand)))
      (cond
       ((zerop (hand-value player-hand)) (format t "Dealer wins~%"))
       ((blackjackp player-hand)
        (format t "Player wins~%")
        (incf *player-purse* (* 2.5 (hand-bet player-hand))))
       (t
	(terpri)
	(let ((dealers-best (dealer-play dealer-hand)))
	  (if (zerop dealers-best)
               (progn
		 (format t "Player wins~%")
		 (incf *player-purse* (* 2 (hand-bet player-hand))))
               (progn
		 (terpri)
		 (show-hands player-hand dealer-hand t)
		 (terpri)
                 (cond
		   ((< dealers-best players-best)
		    (format t "Player wins~%")
		    (incf *player-purse* (* 2 (hand-bet player-hand))))
		   ((> dealers-best players-best)
		    (format t "Dealer wins~%"))
		   (t
		    (format t "Push")
		    (incf *player-purse* (hand-bet player-hand))))))))))))

;; main routine

(defun blackjack ()
  (format t "You have: ~8,2f~%" *player-purse*)
  (play-one-hand)
  (if (< *player-purse* +table-min+)
      (format t  "You're out of money!~%")
      (progn
	(format t "You have: ~8,2f~%" *player-purse*)
	(when (eq (get-response "Continue ([Y]es or [N]) ([Y]N)? "
				"Please answer [Y]es or [N]o (default Y): "
				'(("y" yes) ("n" no)) :default 'yes)
		  'yes)
	  (blackjack))))
  0)
