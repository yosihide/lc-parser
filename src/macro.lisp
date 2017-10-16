(in-package :lc-parser)

;;; Macro
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
		 syms)
     ,@body))

;;; Anaphoric Macro
(defmacro aif (test then &optional else)
  `(let ((it ,test))
    (declare (ignorable it))
    (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
    (progn ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym))
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

(defmacro aand (&rest args)
  (cond
    ((null args)
     t)
    ((null (cdr args))
     (car args))
    (t
     `(aif ,(car args) (aand ,@(cdr args))))))
