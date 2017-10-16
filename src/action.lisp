(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(declaim (inline make-action))
(defun make-action (name argument)
  (list name argument))

(declaim (inline action-name))
(defun action-name (action) (first action))

(declaim (inline action-argument))
(defun action-argument (action) (second action))

(declaim (inline action-match?))
(defun action-match? (actions name argument)
  (when (aand (first actions)
	      (and (eq (action-name it) name)
		   (equal (action-argument it) argument)))
    (rest actions)))
