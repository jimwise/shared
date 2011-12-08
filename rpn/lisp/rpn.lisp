(defvar *ops* (make-hash-table :test 'equal))

(defmacro def-rpn-op (sym args &body bod)
  `(setf (gethash ,sym *ops*)
	 #'(lambda (list)
	   (let ((rest (nthcdr ,(length args) list))
		   ,@(loop for i from 0 to (1- (length args))
			 collect (list (nth i args) (list 'nth i 'list))))
	     (if (some #'null (list ,@args))
		 (progn
		   (signal-error "stack underflow")
		   list)
		 (progn
		 ,@bod))))))

(def-rpn-op "+" (x y) (cons (+ x y) rest))
(def-rpn-op "-" (x y) (cons (- x y) rest))
(def-rpn-op "*" (x y) (cons (* x y) rest))
(def-rpn-op "/" (x y) (cons (/ x y) rest))
(def-rpn-op "^" (x y) (cons (expt y x) rest))
(def-rpn-op "." () (princ (first rest)) (terpri) rest)

(defun op-lookup (sym) (gethash sym *ops*))

(defun rpn-eval (str stack)
  (let ((val (parse-number str))
	(op (op-lookup str)))
    (cond 
      ((numberp val) (cons val stack))
      (op (funcall op stack))
      (t (signal-error "unknown operation") stack))))

(defun signal-error (str)
  (format t "*** ERROR: ~a~%" str))

;; (rpn-eval "*" '(3 2 1))
;; (rpn-eval "/" '(5 2 1))
;; (rpn-eval "+" '(3 2 1))

(defun rpn-repl (&optional stack)
  (format *query-io* "> ")
  (force-output *query-io*)
  (let ((str (read-line *query-io* nil)))
    (if str
      (rpn-repl (rpn-eval str stack))
      (terpri))))

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

(rpn-repl)
