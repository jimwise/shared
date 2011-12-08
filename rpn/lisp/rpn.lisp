(defvar *ops* (make-hash-table :test 'equal))
(defvar *ops-docs* (make-hash-table :test 'equal))

(defmacro def-rpn-op (name args doc &body bod)
  "Define an RPN operation given a name, a doc string, a list of arguments
and a body.  The body is invoked with each argument bound to one of the
first n = (length args) values on the stack, 'rest' bound to the stack with
those argument dropped, and 'stack' bound to the stack without those
arguments dropped.  The body must return the new value of the stack (see
below for examples)."
  `(setf (gethash ,name *ops*)
	 #'(lambda (stack)
	     (let ((rest (nthcdr ,(length args) stack))
		   ,@(loop for i from 0 to (1- (length args))
			collect (list (nth i args) (list 'nth i 'stack))))
	       (declare (ignorable rest))
	       (if (some #'null (list ,@args))
		   (progn
		     (signal-error "stack underflow")
		     stack)
		   (progn
		     ,@bod))))
	 (gethash ,name *ops-docs*) ,doc))

(def-rpn-op "+" (x y)
    "replace top two values on stack with their sum"
  (cons (+ x y) rest))
(def-rpn-op "-" (x y)
    "replace top two values on stack with their difference"
  (cons (- x y) rest))
(def-rpn-op "*" (x y)
    "replace top two values on stack with their product"
  (cons (* x y) rest))
(def-rpn-op "/" (x y)
    "replace top two values on stack with their quotient"
  (cons (/ x y) rest))
(def-rpn-op "^" (x y)
    "replace top two values on stack, x and y with x^y"
  (cons (expt y x) rest))
(def-rpn-op "." (x)
    "display the top value on the stack"
  (princ x)
  (terpri)
  stack)
(def-rpn-op "drop" (x)
    "remove the top value from the stack"
  rest)
(def-rpn-op "dup" (x)
    "duplicate the top value on the stack"
    (cons x stack))
(def-rpn-op "swap" (x y)
    "swap the top two values on the stack"
  (cons y (cons x stack)))
(def-rpn-op "help" ()
    "display this help"
  (format *query-io* "~a Commands:~%" (hash-table-count *ops-docs*))
  (maphash #'(lambda (key val) (format *query-io* "  ~a -- ~a~%" key val)) *ops-docs*) stack)

(defun op-lookup (name) (gethash name *ops*))
(defun doc-lookup (name) (gethash name *ops-docs*))

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
