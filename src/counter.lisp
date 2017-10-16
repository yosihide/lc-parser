(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defun make-counter (&key (test 'equal))
  (make-hash-table :test test))

(declaim (inline counter-inc counter-count counter-elements))
(defun counter-inc (element count counter)
  (declare (fixnum count))
  (incf (the fixnum (gethash element counter 0)) count))

(declaim (inline counter-count))
(defun counter-count (element counter)
  (the fixnum (gethash element counter)))

(defun counter-elements (counter)
  (hash-keys counter))

(defun counter-frequent-elements (counter minsup)
  (declare (fixnum minsup))
  (let ((elements nil))
    (dolist (element (counter-elements counter))
      (when (> (counter-count element counter) minsup)
	(push element elements)))
    elements))
