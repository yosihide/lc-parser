(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(declaim (inline make-context-feauture
		 context-feature-template
		 context-feature-value))

(defun make-context-feature (template value)
  (list template value))

(defun context-feature-template (context-feature)
  (nth 0 context-feature))

(defun context-feature-value (context-feature)
  (nth 1 context-feature))

(declaim (inline make-feature make-features
		 feature-action-name
		 feature-action-argument
		 feature-context-template
		 feature-context-value))

(defun make-feature (action-name argument template feature-value)
  (list action-name argument template feature-value))

(defun make-features (action context-features)
  (mapcar #'(lambda (f) (make-feature (action-name action)
				      (action-argument action)
				      (context-feature-template f)
				      (context-feature-value f)))
	  context-features))

(defun feature-action-name (f) (nth 0 f))
(defun feature-action-argument (f) (nth 1 f))
(defun feature-context-template (f) (nth 2 f))
(defun feature-context-value (f) (nth 3 f))
