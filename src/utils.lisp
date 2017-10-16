(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

;;; Misc.
(defun suc-find-if (successor f element)
  (declare (function successor f))
  (when element
    (cond
      ((funcall f element)
       element)
      (t
       (suc-find-if successor f (funcall successor element))))))

