(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(declaim (inline make-tree))
(defun make-tree (root-label subtrees)
  (cons root-label subtrees))

(declaim (inline root-label))
(defun root-label (tree) (car tree))
(defmethod label (tree) (root-label tree))

(declaim (inline subtrees))
(defun subtrees (tree) (cdr tree))

(declaim (inline preterminal?))
(defun preterminal? (tree)
  (atom (cadr tree)))

(declaim (inline terminal))
(defun terminal (tree)
  (if (preterminal? tree)
      (cadr tree)
      (error "~s is not preterminal." tree)))

(defun preterminal-trees (tree)
  (cond
    ((preterminal? tree)
     (list tree))
    (t
     (mapcan #'preterminal-trees (subtrees tree)))))

(defun terminals (tree)
  (let ((result nil))
    (labels
	((scan-and-push (tree)
	   (cond
	     ((preterminal? tree)
	      (push (cadr tree) result))
	     (t
	      (dolist (x (subtrees tree))
		(scan-and-push x))))))
      (scan-and-push tree)
      (nreverse result))))

(defun sentence (tree)
  (remove 0 (terminals tree)))

(defmacro tree-ref (tree &rest indexes)
  (if (null indexes)
      tree
      `(tree-ref (nth ,(car indexes) (subtrees ,tree)) ,@(cdr indexes))))

(defun remove-nodes-if (test tree)
  (declare (function test))
  (cond
    ((funcall test tree)
     nil)

    ((preterminal? tree)
     tree)

    (t
     (let ((subtrees (delete nil
			     (mapcar #'(lambda (x) (remove-nodes-if test x))
				     (subtrees tree)))))
       (if (null subtrees)
	   nil
	   (make-tree (root-label tree) subtrees))))))


;;; parse label
(defun make-char-buffer nil
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun push-char-buffer (x buf)
  (vector-push-extend x buf))

(defun get-char-buffer (buf)
  (let ((str (make-array (length buf) :element-type 'character)))
    (dotimes (i (length buf))
      (setf (char str i) (char buf i)))
    (setf (fill-pointer buf) 0)
    str))

(defun parse-label (label)
  (let ((str (string label))
	(category nil)
	(functions nil)
	(-index nil)
	(=index nil)

	(state :start)
	(buf (make-char-buffer)))

    (if (char= (char str 0) #\-)
	(return-from parse-label
	  (list (cons :category str))))

    (labels
	((extract-info nil
	   (case state
	     (:start
	      (setf category (get-char-buffer buf)))
	     (#\-
	      (let* ((token (get-char-buffer buf))
		     (object (read-from-string token)))
		(cond
		  ((numberp object)
		   (setf -index object))
		  (t
		   (push token functions)))))
	     (#\=
	      (setf =index  (read-from-string (get-char-buffer buf)))))))

      (dotimes (i (length str))
	(cond
	  ((member (char str i) '(#\- #\=) :test #'char=)
	   (extract-info)
	   (setf state (char str i)))
	  (t
	   (push-char-buffer (char str i) buf))))

      (extract-info))

    (delete nil
	    (list (cons :category category)
		  (if functions (cons :functions (nreverse functions)))
		  (if -index (cons :-index -index))
		  (if =index (cons :=index =index))))))


;;; reader
(defvar +eos+ (gensym))

(defun whitespace? (x)
  (member x '(#\Linefeed #\Newline #\Page #\Return #\Space #\Tab) :test #'char=))

(defun skip-whitespace (stream)
  (loop
     :while (whitespace? (peek-char nil stream nil #\a))
     :do
     (read-char stream)))

(defun read-terminal (stream)
  (let ((buf (make-char-buffer)))
    (loop
       :while (not (char= (peek-char t stream) #\) ))
       :do
       (if (char= (peek-char t stream) #\\)
	   (read-char stream)
	   (vector-push-extend (read-char stream) buf)))
    buf))

(defun read-label (stream)
  (let ((buf (make-char-buffer)))
    (loop
       :while (not ((lambda (x) (or (whitespace? x) (char= x #\( ))) (peek-char nil stream)))
       :do
       (vector-push-extend (read-char stream) buf))
    buf))

(defun read-tree-aux (stream)

  (skip-whitespace stream)

  (if (char= (read-char stream) #\()

      (let ((root nil)
	    (subtrees nil))

	(skip-whitespace stream)

	(cond
	  ((char= (peek-char nil stream) #\()
	   (setf root "TOP"))
	  (t
	   (setf root (read-label stream))))

	(skip-whitespace stream)

	(cond
	  ((char= (peek-char t stream) #\()
	   (loop
	      :while (not (char= (peek-char t stream) #\)))
	      :do
	      (push (read-tree-aux stream) subtrees))
	   (setf subtrees (nreverse subtrees)))
	  (t
	   (setf subtrees (list (read-terminal stream)))))
	(skip-whitespace stream)
	(read-char stream)
	(cons (intern root *package*) subtrees))

      (error t "error in read-tree.")))

(defun read-tree (&optional (stream t) (eof-error-p t) eof-value)
  (skip-whitespace stream)

  (if (and (not eof-error-p)
	   (eq (peek-char nil stream nil +eos+) +eos+))
      (return-from read-tree eof-value)
      (read-tree-aux stream)))

(defmacro do-stream-tree ((x stream &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(let ((,gstrm ,stream))
      (do ((,x))
	  ((eq (setf ,x (read-tree ,gstrm nil +eos+)) +eos+) ,result-form)
	,@body))))

(defmacro do-file-tree ((x path &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(with-open-file (,gstrm ,path)
      (do-stream-tree (,x ,gstrm ,result-form) ,@body))))

(defun read-trees (file)
  (let ((trees nil))
    (do-file-tree (x file)
      (push x trees))
    (nreverse trees)))

(defun write-trees (trees output &key (root ""))
  (with-open-file (out output :direction :output)
    (dolist (tree trees)
      (labels
	  ((aux (tree)
	     (format out " (~a" (symbol-name (root-label tree)))
	     (cond
	       ((preterminal? tree)
		(format out " ~a)" (terminal tree)))
	       (t
		(dolist (x (subtrees tree))
		  (aux x))
		(format out ")")))))

	(format out "(")
	(format out "~a" root)
	(dolist (x (subtrees tree))
	  (aux x))
	(format out ")~%")))))
