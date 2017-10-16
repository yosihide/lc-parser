(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(declaim (inline hash-keys))
(defun hash-keys (hash)
  (let ((result nil))
    (maphash #'(lambda (key val) (declare (ignore val)) (push key result))
	     hash)
    result))

(defun hash-table->list (hash)
  (let ((result nil))
    (maphash #'(lambda (key val) (push (cons key val) result))
	     hash)
    result))

(defun table->entries (name table)
  (mapcar #'(lambda (x) (cons name x)) (hash-table->list table)))
