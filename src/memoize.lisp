(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defun declare? (x)
  (cond
    ((atom x) nil)
    ((eq (car x) 'declare) t)
    (t nil)))

(defun parse-body (body)
  (do* (head
	doc-exist?
	(rest body (cdr rest))
	(x (car rest) (car rest)))
       (nil)
    (cond
      ((declare? x)
       (setf head (append head (list x))))
      ((stringp x)
       (cond
	 (doc-exist?
	  (return-from parse-body (values head rest)))
	 (t
	  (setf doc-exist? t)
	  (setf head (append head (list x))))))
      (t
       (return-from parse-body (values head rest))))))

(defun name-and-options (x)
  (etypecase x
    (symbol (list x nil))
    (list (if (cdr x)
	      x
	      (append x (list nil))))))

(defun memo-declaration (key key-val)
  `(,(if (equal key '#'car) 'ignore 'function) ,key-val))

(defun cache-key-binding (args key key-val cache-key)
  (cond
    ((equal key '#'car)
     `(,cache-key ,(car args)))
    ((equal key '#'list*)
     `(,cache-key (list* ,@args)))
    (t
     `(,cache-key (funcall ,key-val (list ,@args))))))


(defun memo-body (args body key key-val cache)
  (let ((val (gensym))
	(present? (gensym))
	(cache-key (gensym)))
    `(let (,(cache-key-binding args key key-val cache-key))
      (multiple-value-bind (,val ,present?) (gethash ,cache-key ,cache)
	(if ,present?
	    ,val
	    (setf (gethash ,cache-key ,cache)
		  (progn ,@body)))))))


(defparameter *cache* (make-hash-table :test 'eq))

(defmacro def-memoized-function (name-and-options args &body body)
  (let ((key-val (gensym))
	(cache (gensym)))
    (destructuring-bind (name (&key (key '#'identity) (test ''equal)))
	(name-and-options name-and-options)

      (multiple-value-bind (head exprs) (parse-body body)

	`(let ((,key-val ,key)
	       (,cache (make-hash-table :test ,test)))
	  (declare ,(memo-declaration key key-val))

	  (setf (gethash ',name *cache*) ,cache)
	  (defun ,name ,args
	    ,@head
	    ,(memo-body args exprs key key-val cache)
	    ))))))


(declaim (inline clr-cache))
(defun clr-cache (f)
  (multiple-value-bind (val present?) (gethash f *cache*)
    (cond
      (present?
       (clrhash val)
       t)
      (t
       nil))))
