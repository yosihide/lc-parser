(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defstruct vector-element
  (weight 0.0 :type single-float)
  (bias 0.0 :type single-float))

(defstruct perceptron
  (vector (make-hash-table :test 'equal))
  (age 0 :type fixnum))

(declaim (inline rare-feature))
(defun rare-feature (f)
  (make-feature (feature-action-name f)
		(feature-action-argument f)
		(feature-context-template f)
		:rare))

(defun create-perceptron (features)
  (let ((vector (make-hash-table :test 'equal)))
    (dolist (f features)
      (setf (gethash f vector) (make-vector-element)))
    (make-perceptron :vector vector)))

(defun perceptron-vector-element (feature perceptron)
  (let ((vector (perceptron-vector perceptron))
	(rare-feature nil))
    (acond
     ((gethash feature vector)
      it)
     (t
      (setf rare-feature (rare-feature feature))
      (acond
       ((gethash rare-feature vector)
	it)
       (t
	(setf (gethash rare-feature vector)
	      (make-vector-element))))))))

; Weights are represented as a simple-vector of single floats.
(defun perceptron-score (features perceptron)

  (when (null perceptron)
    (return-from perceptron-score 0.0))

  (let ((score 0.0))
    (declare (single-float score))
    (dolist (f features)
      (incf score (vector-element-weight (perceptron-vector-element f perceptron))))
    score))

;;; Update
(defun perceptron-update! (perceptron positive negative)

  (dolist (features positive)
    (dolist (f features)
      (let ((element (perceptron-vector-element f perceptron)))
	(incf (vector-element-weight element))
	(incf (vector-element-bias element) (perceptron-age perceptron)))))

  (dolist (features negative)
    (dolist (f features)
      (let ((element (perceptron-vector-element f perceptron)))
	(decf (vector-element-weight element))
	(decf (vector-element-bias element) (perceptron-age perceptron)))))
  t)

(defun averaged-perceptron (perceptron)
  (let ((vector (make-hash-table :test 'equal))
	(age (perceptron-age perceptron)))
    (dolist (f (hash-keys (perceptron-vector perceptron)))
      (let* ((element (gethash f (perceptron-vector perceptron)))
	     (averaged-weight (- (vector-element-weight element)
				 (/ (vector-element-bias element) age))))
	(setf (gethash f vector)
	      (make-vector-element :weight averaged-weight))))
    (make-perceptron :vector vector)))

(defun perceptron-save (perceptron file)
  (let ((*print-pretty* nil))
    (write-objects (append (list (cons :age (perceptron-age perceptron)))
			   (let ((result nil))
			     (maphash
			      #'(lambda (f v)
				  (push (list :vector-element f (vector-element-weight v) (vector-element-bias v))
					result))
			      (perceptron-vector perceptron))
			     result))
		   file)))

(defun perceptron-load (file)
  (let ((objects (read-objects file))
	(age 0)
	(vector (make-hash-table :test 'equal)))
    (dolist (obj objects)
      (case (car obj)
	(:age
	 (setf age (cdr obj)))
	(:vector-element
	 (setf (gethash (nth 1 obj) vector)
	       (make-vector-element :weight (nth 2 obj) :bias (nth 3 obj))))))
    (make-perceptron :vector vector
		     :age age)))
