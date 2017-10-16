(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defvar +eos+ (gensym))

(defmethod read-objects ((x stream))
  (do ((y)
       (result))
      ((eq (setf y (read x nil +eos+)) +eos+) (nreverse result))
    (push y result)))

(defmethod read-objects ((x string))
  (with-open-file (in x)
    (read-objects in)))

(defmethod read-objects ((x pathname))
  (with-open-file (in x)
    (read-objects in)))


(defmethod write-objects (objects (x stream))
  (dolist (y objects)
    (format x "~S~%" y)))

(defmethod write-objects (objects (x string))
  (with-open-file (out x :direction :output :if-does-not-exist :create)
    (write-objects objects out)))

(defmethod write-objects (objects (x pathname))
  (with-open-file (out x :direction :output :if-does-not-exist :create)
    (write-objects objects out)))
