(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defun str (&rest args)
  (apply #'concatenate 'string args))

(define-compiler-macro str (&rest args)
  `(concatenate 'string ,@args))
