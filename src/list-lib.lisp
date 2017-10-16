(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

;;; List
(declaim (inline append1 last1))
(defun append1 (lst x) (append lst (list x)))
(defun last1 (lst) (car (last lst)))

(declaim (inline single?))
(defun single? (lst) (and (consp lst) (null (cdr lst))))

(defun mappend (f &rest lsts)
  (apply #'append (apply #'mapcar f lsts)))

(define-compiler-macro mappend (f &rest lsts)
  `(apply #'append (mapcar ,f ,@lsts)))

(defun n-best (sorted-list n)
  (declare (fixnum n))
  (labels
      ((aux (list n acc)
	 (cond
	   ((or (null list) (= n 0))
	    (nreverse acc))
	   (t
	    (aux (rest list) (- n 1) (cons (first list) acc))))))
    (aux sorted-list n nil)))

(defun sublist (list test)
  (declare (function test))
  (let ((result nil))
    (labels
	((aux (list acc)
	   (cond
	     ((null list)
	      (when acc
		(push (nreverse acc) result)))
	     ((funcall test (first list))
	      (aux (rest list) (cons (first list) acc)))
	     (t
	      (when acc
		(push (nreverse acc) result))
	      (aux (rest list) nil)))))
      (aux list nil))
    (nreverse result)))

;;; Alist
(declaim (inline alist-get))
(defun alist-get (key alist &key (test #'eql))
  (cdr (assoc key alist :test test)))

(defun alist-update (key value alist &key (test #'eql))
  (declare (function test))
  (cond
    ((null alist)
     (list (cons key value)))
    ((funcall test key (caar alist))
     (acons (caar alist)
	    value
	    (cdr alist)))
    (t
     (cons (car alist)
	   (alist-update key value (cdr alist) :test test)))))

(defun alist-push (key value alist &key (test #'eql))
  (declare (function test))
  (cond
    ((null alist)
     (list (cons key (list value))))
    ((funcall test key (caar alist))
     (acons (caar alist)
	    (cons value (cdar alist))
	    (cdr alist)))
    (t
     (cons (car alist)
	   (alist-push key value (cdr alist) :test test)))))

(defun alist-pushnew (key value alist &key (key-test #'eql) (value-test #'eql))
  (declare (function key-test value-test))
  (cond
    ((null alist)
     (list (cons key (list value))))
    ((funcall key-test key (caar alist))
     (if (member value (cdar alist) :test value-test)
	 alist
	 (acons (caar alist)
		(cons value (cdar alist))
		(cdr alist))))
    (t
     (cons (car alist)
	   (alist-pushnew key value (cdr alist) :key-test key-test :value-test value-test)))))
