(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct memo
    action
    features
    (score 0.0 :type single-float)))

(defconstant +null-memo+ (make-memo))

(defun make-transition-function (grammar perceptron len)
  (declare (fixnum len))
  (let ((surface-context-features-vec (make-array (1+ len)))
	(position -1)
	(surface-table (make-hash-table :test 'equal))
	(local-table (make-hash-table :test 'equal))
	(nld-table (make-hash-table :test 'equal)))
    (declare (fixnum position))
    (labels
	((surface-context-features (state)
	   (let ((i (state-position state)))
	     (cond
	       ((<= i position)
		(svref surface-context-features-vec i))
	       (t
		(setf position i)
		(setf (svref surface-context-features-vec i)
		      (state-surface-context-features state))))))

	 (surface-memo (action state)
	   (let ((key (cons action (state-position state))))
	     (acond
	      ((gethash key surface-table)
	       it)
	      (t
	       (let ((surface-features (make-features action (surface-context-features state))))
		 (setf (gethash key surface-table)
		       (make-memo :features surface-features
				  :score (perceptron-score surface-features perceptron))))))))

	 (memo (state)
	   (let ((key (equivalence-key state)))
	     (acond
	      ((gethash key local-table)
	       it)
	      (t
	       (let ((actions (allowable-actions state grammar)))
		 (cond
		   ((and (single? actions)
			 (eq (action-name (first actions)) :idle))
		    (setf (gethash key local-table)
			  (list (make-memo :action (first actions)))))
		   (t
		    (let ((local-context-features (state-local-context-features state)))
		      (setf (gethash key local-table)
			    (mapcar #'(lambda (a)
					(let ((local-features (make-features a local-context-features))
					      (surface-memo (surface-memo a state)))
					  (make-memo :action a
						     :features (append local-features (memo-features surface-memo))
						     :score (+ (perceptron-score local-features perceptron)
							       (memo-score surface-memo)))))
				    actions))))))))))

	 (nld-memo (action nld-context-features nld-equivalence-key)
	   (let ((key (cons action nld-equivalence-key)))
	     (acond
	      ((gethash key nld-table)
	       it)
	      (t
	       (let ((features (make-features action nld-context-features)))
		 (setf (gethash key nld-table)
		       (make-memo :features features
				  :score (perceptron-score features perceptron))))))))

	 (expand (state)
	   (let ((result nil)
		 (nld-context-features (state-nld-context-features state))
		 (nld-equivalence-key (nld-equivalence-key state)))

	     (dolist (memo (memo state))
	       (let* ((action (memo-action memo))
		      (action-name (action-name action))
		      (action-argument (action-argument action)))
		 (cond
		   ((and (eq action-name :shift)
			 (= (state-e-num state) (grammar-e-max grammar))
			 (NONE? action-argument))
		    nil)

		   ((eq action-name :idle)
		    (let ((features (make-features action (list nil))))
		      (push (state-idle state :features-set (list features) :score (perceptron-score features perceptron))
			    result)))

		   ((eq action-name :left-corner)
		    (let ((nld-memo +null-memo+))

		      (when (grammar-nld-feature grammar)
			(setf nld-memo (nld-memo action nld-context-features nld-equivalence-key)))

		      (push (state-left-corner state action-argument
					       :features-set (list (memo-features nld-memo) (memo-features memo))
					       :score (+ (memo-score nld-memo) (memo-score memo)))
			    result)))

		   ((eq action-name :attach)
		    (let* ((features nil)
			   (nld-memo +null-memo+)
			   (right-references nil)
			   (root-references nil)
			   (rule action-argument)
			   (label (attach-rule-label rule)))
		      (cond
			((FIL? label)
			 (when (eq (nonlocal-dependency-direction label) :R)
			   (let ((f-cat (nonlocal-dependency-category label))
				 (f-type (nonlocal-dependency-type label)))
			     (cond
			       ((eq f-type :*RNR*)
				(setf right-references (find-all-empty-elements state f-cat f-type)))
			       ((eq f-type :*EXP*)
				(awhen (find-*EXP*-NP state)
				  (setf right-references (list it))))
			       (t
				(awhen (find-empty-element state f-cat f-type)
				  (setf right-references (list it))))))

			   (when (grammar-nld-feature grammar)
			     (setf features (make-nld-identification-features action (if (or right-references root-references) t))))))

			((PRN? label)
			 (setf root-references (find-*T*-empty-elements state)))

			((SBJ? label)
			 (setf right-references (find-all-empty-elements state nil :*))))

		      (when (grammar-nld-feature grammar)
			(setf nld-memo (nld-memo action nld-context-features nld-equivalence-key)))

		      (push (state-attach state rule
					  :right-references right-references
					  :root-references root-references
					  :features-set (list features (memo-features nld-memo) (memo-features memo))
					  :score (+ (perceptron-score features perceptron) (memo-score nld-memo) (memo-score memo)))
			    result)))

		   ((eq action-name :shift)
		    (let ((features nil)
			  (nld-memo +null-memo+)
			  (filler nil)
			  (label (action-argument action)))

		      (when (eq (nonlocal-dependency-direction label) :L)
			(let ((e-cat (nonlocal-dependency-category label))
			      (e-type (nonlocal-dependency-type label)))
			  (cond
			    ((eq e-type :*)
			     (cond
			       ((OBJCTRL? label)
				(setf filler (find-OBJ-controller state)))
			       (t
				(setf filler (find-SBJ-controller state)))))
			    ((eq e-type :*T*)
			     (setf filler (find-*T*-filler state e-cat)))
			    (t
			     (setf filler (find-filler state e-cat e-type))))

			  (when (grammar-nld-feature grammar)
			    (setf features (make-nld-identification-features action (if filler t))))))

		      (when (grammar-nld-feature grammar)
			(setf nld-memo (nld-memo action nld-context-features nld-equivalence-key)))

		      (push (state-shift state label
					 :references (if filler (list filler))
					 :features-set (list features (memo-features nld-memo) (memo-features memo))
					 :score (+ (perceptron-score features perceptron) (memo-score nld-memo) (memo-score memo)))
			    result))))))
	     result))

	 (transition (states beam)
	   (declare (fixnum beam))
	   (let ((promises (sort (mappend #'expand states) #'> :key #'promise-score)))
	     (values (mapcar #'force-promise (n-best promises beam))
		     (aif (find-if #'promise-actions promises) (force-promise it))
		     (awhen (remove-if #'promise-actions promises)
		       (force-promise (first it)))))))
      #'transition)))

(defun extract-features-set (tree grammar)
  (let* ((actions (append1 (actions tree) (make-action :end nil)))
	 (sentence (sentence tree))
	 (state (make-state :sentence (map 'simple-vector #'(lambda (x) (analyze-word x grammar)) sentence)
			    :actions actions))
	 (transition (make-transition-function grammar nil (length sentence))))
    (loop
       :while state
       :do
       (setf state (nth-value 1 (funcall transition (list state) 0))) 
       (when (and state
		  (eq (action-name (first (state-actions state))) :end))
	 (return-from extract-features-set
	   (cons (make-features (make-action :idle nil) (list nil))
		 (state-extract-features-set state)))))))


(defun extract-frequent-features (corpus grammar minsup)
  (declare (fixnum minsup))
  (let ((counter (make-counter :test 'equal)))
    (dolist (tree corpus)
      (dolist (features (extract-features-set tree grammar))
	(dolist (f features)
	  (counter-inc f 1 counter))))

    (counter-frequent-elements counter minsup)))

(defun find-max-violation-pair (positives negatives)
  (let ((diff 0.0)
	(min-diff most-positive-single-float)
	(pair nil))
    (declare (single-float diff min-diff))
    (loop
       :for pos :in (reverse positives)
       :for neg :in (reverse negatives)
       :do
       (when (and pos neg)
	 (setf diff (- (state-score pos) (state-score neg)))
	 (when (< diff min-diff)
	   (setf pair (list pos neg))
	   (setf min-diff diff))))

    (when (<= min-diff 0.0) pair)))


(defun parser (sentence grammar perceptron &key (training-data nil) (beam 16))
  (declare (fixnum beam))
  (let* ((transition (make-transition-function grammar perceptron (length sentence)))
	 (actions (when training-data
		    (append1 (actions training-data) (make-action :end nil))))
	 (initial-states (list (make-state :sentence (map 'simple-vector #'(lambda (x) (analyze-word x grammar)) sentence)
					   :actions actions)))

	 ; The variable "best-state" is used for parsing.
	 (best-state (first initial-states))

	 ; The variables "negatives" and "positives" are used for training.
	 (negatives nil)
	 (positives nil))

    (let ((states initial-states))
      (loop
	 :repeat (* (grammar-ratio grammar) (length sentence))
	 :do
	 (multiple-value-bind (new-states pos neg) (funcall transition states beam)

	   (setf states new-states)

	   (when states
	     (setf best-state (first states)))

	   (when (not training-data)
	     (dolist (s states)
	       (setf (state-features-set s) nil)))

	   (when pos (push pos positives))
	   (push neg negatives))))

    (when training-data
      (let ((states (list (or (first positives) (first initial-states)))))
	(loop
	   :repeat (- (* (grammar-ratio grammar) (length sentence)) (length positives))
	   :do
	   (multiple-value-bind (new-states pos neg) (funcall transition states 0)
	     (declare (ignore new-states neg))
	     (push pos positives)
	     (setf states (if pos (list pos)))))))

    (cond
      ;; parsing mode
      ((null training-data)
       (return-from parser (if best-state (ptree->tree (ptree-recover-nonlocal-dependency (state-get-ptree best-state (grammar-top grammar)))))))

      ;; training mode
      (t
       (acond
	((find-max-violation-pair positives negatives)
	 (perceptron-update! perceptron (state-extract-features-set (nth 0 it)) (state-extract-features-set (nth 1 it)))
	 (return-from parser :update))
	(t
	 (return-from parser nil)))))))


(defun train (&key training-corpus grammar features iter beam data-dir)

  (let ((perceptron (create-perceptron features)))

    (perceptron-save perceptron (merge-pathnames data-dir (make-pathname :name "perceptron-init")))

    (loop
       :for pass :of-type fixnum :from 1 :to iter
       :do
       (let ((update 0)
	     (start (get-internal-run-time))
	     (end nil))
	 (declare (fixnum update))
	 (loop
	    :for tree :in training-corpus
	    :for i :of-type fixnum :from 1
	    :do
	    (let ((result (parser (sentence tree) grammar perceptron :training-data tree :beam beam)))
	      (incf (perceptron-age perceptron))
	      (when (eq result :update)
		(incf update))

	      (when (= (mod i 1000) 0)
		(setf end (get-internal-run-time))
		(format t "~s(time:~,1f sec.) update:~s~%"
			i
			(/ (- end (float start)) internal-time-units-per-second)
			update)
		(setf start end)
		(force-output))))

	 (perceptron-save perceptron
			  (merge-pathnames data-dir (make-pathname :name (format nil "perceptron~s" pass))))))))

(defun gold-standard (tree grammar)
  (let* ((sentence (sentence tree))
	 (transition (make-transition-function grammar (make-perceptron) (length sentence)))
	 (actions (append1 (actions tree) (make-action :end nil)))
	 (states (list (make-state :sentence (map 'simple-vector #'(lambda (x) (analyze-word x grammar)) sentence)
				   :actions actions))))
    (loop
       :repeat (* (grammar-ratio grammar) (length sentence))
       :do
       (multiple-value-bind (new-states pos neg) (funcall transition states 0)
	 (declare (ignore new-states neg))
	 (setf (state-features-set pos) nil)
	 (setf states (list pos))))
    (return-from gold-standard
      (ptree->tree (ptree-recover-nonlocal-dependency (state-get-ptree (first states) (grammar-top grammar)))))))

(defun gold-standard-results (&key trees file (verbose t))
  (let ((i 0)
	(grammar (extract-grammar (append trees trees) 0 t)) ;; note: extract-grammar remove the elements which occur only once.
	(result nil)
	(results nil)
	(*print-pretty* nil))

    (declare (fixnum i))

    (dolist (tree trees)
      (setf result (gold-standard tree grammar))
      (push result results)

      (when verbose
	(incf i)
	(print i)
	(print result)
	(force-output)))

    (write-objects (nreverse results) file))

  nil)
