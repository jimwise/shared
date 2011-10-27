(defpackage :blackio
  (:use :common-lisp)
  (:export :get-response
	   :get-number))

(in-package :blackio)

;; getresp : string string list-of-pair symbol
;; allowed is an alist of ( string symbol )
;; prompt with prompt1, and get a response character, returning the matching symbol
;; from allowed, or `default' if user hits <return>.
;; if user enters a disallowed character, loop, prompting with prompt2

(defun get-response (prompt reprompt allowed &key default)
  (format *query-io* prompt)
  (force-output *query-io*)
  (loop
      (let ((c (assoc (read-line *query-io*) allowed :test #'string-equal)))
	(if c
	    (return (second c))
	    (if default
		(return default)
		(princ reprompt))))))

; example
; (print (getresp "foo (y/n): " "bar (y/n): " '(("y" yes) ("n" no)) :default 'yes))

;; getnum : num num num num string -> num
;; prompts for a number between min and max, integer divisible by step, and keeps trying until
;; it gets one.  if user hits return, use default

(defun get-number (prompt &key min max step default)
  (format *query-io* prompt)
  (force-output *query-io*)
  (loop
       (let ((l (read-line *query-io*)))
	 (if (and default (string= l ""))
	     (return default)
	     (let ((n (parse-number l)))
	       (cond
		 ((not n)
		  (princ "Amount must be a number, try again: "))
		 ((and min (< n min))
		  (format t "Amount must be at least ~8,2F, try again: " min))
		 ((and max (> n max))
		  (format t "Amount must bey at most ~8,2F, try again: " max))
		 ((and step (not (= 0 (mod n step))))
		  (format t "Amount must be in increments of ~8,2F, try again: " step))
		 (t (return n))))))))
;example
;(princ (getnum "foobar: " :min 5 :max 1000 :step 5 :default 10))aa

;; (define (read-line/default default)
;;   (let ([l (get-line (current-input-port))])
;;     (if (string=? l "") default l)))
  

(defun parse-number (str)
  (let ((n (safely-read-from-string str)))
    (if (numberp n) n nil)))

;; from http://paste.org/pastebin/view/7046
(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set
to NIL. Any unsafe expressions will be replaced by NIL in the
resulting S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))
