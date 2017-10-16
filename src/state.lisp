(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defstruct state
  ptree
  predictor
  (sentence #() :type simple-vector)
  (position 0 :type fixnum)
  (e-num 0 :type fixnum)
  features-set
  (score 0.0 :type single-float)
  actions

  c-commanding-SBJ
  c-commanding-OBJ

  nld-elements
  s0.nld-elements
  s1.nld-elements
  rest.nld-elements
  )

(declaim (inline state-bottom?))
(defun state-bottom? (state)
  (and (state-p state) (null (state-ptree state))))

(declaim (inline state-final?))
(defun state-final? (state top)
  (and (state-bottom? (state-predictor state))
       (= (state-position state) (length (state-sentence state)))
       (eq (ptree-root-label (state-ptree state)) top)))

(defun state-ref (state i)
  (declare (fixnum i))
  (cond
    ((null state)
     nil)
    ((= i 0)
     (state-ptree state))
    ((> i 0)
     (state-ref (state-predictor state) (- i 1)))
    (t
     (error "~s is nagative." i))))

(defconstant +end+ (make-word))

(declaim (inline state-nth-word))
(defun state-nth-word (i state)
  (declare (fixnum i))
  (let ((sentence (state-sentence state)))
    (cond
      ((< -1 i (length sentence))
       (svref sentence i))
      (t
       +end+))))

(defun state-extract-features-set (state &optional acc)
  (cond
    ((null state)
     acc)
    (t
     (state-extract-features-set (state-predictor state)
				 (append (state-features-set state) acc)))))

(declaim (inline new-state))
(defun new-state (&key predictor ptree sentence position e-num features-set score actions)
  (let ((state nil)
	(nld-elements nil))
    (setf state
	  (make-state :ptree ptree
		      :predictor predictor
		      :sentence sentence
		      :position position
		      :e-num e-num
		      :features-set features-set
		      :score score
		      :actions actions

		      :c-commanding-SBJ (or (ptree-c-commanding-SBJ ptree) (state-c-commanding-SBJ predictor))
		      :c-commanding-OBJ (or (ptree-c-commanding-OBJ ptree) (state-c-commanding-OBJ predictor))
		      ))

    (setf nld-elements (reduce-nld-elements state))

    (setf (state-s0.nld-elements state)
	  (remove-if-not #'(lambda (x) (member x (ptree-nld-elements ptree) :test #'eq))
			 nld-elements))

    (awhen (state-ref state 1)
      (setf (state-s1.nld-elements state)
	    (remove-if-not #'(lambda (x) (member x (ptree-nld-elements it) :test #'eq))
			   nld-elements)))

    (setf (state-rest.nld-elements state)
	  (remove-if #'(lambda (x) (or (member x (ptree-nld-elements ptree) :test #'eq)
				       (member x (state-s1.nld-elements state) :test #'eq)))
		     nld-elements))

    (setf (state-nld-elements state) nld-elements)

    state))

(defstruct promise
  closure
  (score 0.0 :type single-float)
  actions
  )

(declaim (inline force-promise))
(defun force-promise (promise)
  (cond
    ((not (promise-p promise))
     promise)
    (t
     (funcall (promise-closure promise)))))

(defun state-idle (state &key (features-set nil) (score 0.0))
  (declare (single-float score))
  (let ((new-score (+ (state-score state) score)))
    (declare (single-float new-score))
    (make-promise
     :closure #'(lambda ()
		  (let ((new-state (copy-state state)))
		    (setf (state-features-set new-state)
			  (append features-set (state-features-set state)))
		    (setf (state-score new-state) new-score)
		    new-state))
     :score new-score
     :actions (state-actions state))))

(defun state-left-corner (state rule &key features-set (score 0.0))
  (declare (single-float score))
  (let ((new-score (+ (state-score state) score))
	(new-actions (action-match? (state-actions state) :left-corner rule)))
    (declare (single-float new-score))
    (make-promise
     :closure #'(lambda ()
		  (new-state :predictor (state-predictor state)
			     :ptree (ptree-left-corner (state-ptree state) rule)
			     :sentence (state-sentence state)
			     :position (state-position state)
			     :e-num (state-e-num state)
			     :features-set (append (state-features-set state) features-set)
			     :score new-score
			     :actions new-actions))
     :score new-score
     :actions new-actions)))

(defun state-attach (state rule &key right-references root-references features-set (score 0.0))
  (declare (single-float score))
  (let ((new-score (+ (state-score state) score))
	(new-actions (action-match? (state-actions state) :attach rule)))
    (declare (single-float new-score))
    (make-promise
     :closure #'(lambda ()
		  (let ((prev-state (state-predictor state)))
		    (new-state :predictor (state-predictor prev-state)
			       :ptree (ptree-attach (state-ptree prev-state) (state-ptree state) rule
						    :right-references right-references
						    :root-references root-references)
			       :sentence (state-sentence state)
			       :position (state-position state)
			       :e-num (state-e-num state)
			       :features-set (append (state-features-set prev-state) (state-features-set state) features-set)
			       :score new-score
			       :actions new-actions)))
     :score new-score
     :actions new-actions)))

(defun state-shift (state label &key references features-set (score 0.0))
  (declare (single-float score))
  (let ((new-score (+ (state-score state) score))
	(new-actions (action-match? (state-actions state) :shift label)))
    (declare (single-float new-score))
    (make-promise
     :closure #'(lambda ()
		  (new-state :predictor state
			     :ptree (make-ptree :root-label label
						:word (cond
							((NONE? label)
							 (analyze-word 0 nil))
							(t
							 (state-nth-word (state-position state) state)))
						:complete? t
						:id (cond
						      ((state-ptree state)
						       (1+ (ptree-last-id (state-ptree state))))
						      (t
						       0))
						:references references)
			     :sentence (state-sentence state)
			     :position (cond
					 ((NONE? label)
					  (state-position state))
					 (t
					  (1+ (state-position state))))
			     :e-num (cond
				      ((NONE? label)
				       (1+ (state-e-num state)))
				      (t
				       0))
			     :features-set features-set
			     :score new-score
			     :actions new-actions))
     :score new-score
     :actions new-actions)))

(defun decompose (state)
  (let ((ptree (state-ptree state)))
    (cond

      ;; shift
      ((ptree-preterminal? ptree)
       (values (make-action :shift (ptree-root-label ptree))
	       (state-predictor state)))

      ;; left-corner
      ((single? (ptree-subtrees ptree))
       (let ((ptree0 (nth 0 (ptree-subtrees ptree))))
	 (values (make-action :left-corner
			      (make-left-corner-rule (ptree-root-label ptree)
						     (remove-bottom-features (ptree-root-label ptree0))
						     (ptree-complete? ptree)))
		 (make-state :predictor (state-predictor state)
			     :ptree (make-ptree :root-label (remove-top-features (ptree-root-label ptree0))
						:subtrees (ptree-subtrees ptree0)
						:word (ptree-word ptree0)
						:complete? t)))))

      ;; attach
      (t
       (let ((ptree1 (make-ptree :root-label (ptree-root-label ptree)
				 :subtrees (rest (ptree-subtrees ptree))
				 :complete? nil))
	     (ptree0 (first (ptree-subtrees ptree))))
	 (values (make-action :attach
			      (make-attach-rule (remove-bottom-features (ptree-root-label ptree0))
						(ptree-complete? ptree)))
		 (make-state :predictor (make-state :predictor (state-predictor state)
						    :ptree ptree1)
			     :ptree (ptree-remove-top-features ptree0))))))))

(defun actions (tree)
  (let ((actions nil))
    (loop
       :with state := (make-state :predictor (make-state) :ptree (tree->ptree tree))
       :with a := nil
       :while (state-predictor state)
       :do
       (multiple-value-setq (a state) (decompose state))
       (push a actions))
    actions))

(defun apply-action (action state)
  (force-promise
   (case (action-name action)
     (:left-corner (state-left-corner state (action-argument action)))
     (:attach (state-attach state (action-argument action)))
     (:shift (state-shift state (action-argument action))))))

;;; nonlocal dependency identification
(defun reduce-nld-elements (state)
  (let ((references nil)
	(nld-elements nil))
    (labels
	((push-nld-element (x)
	   (cond
	     ((ptree-references x)
	      (setf references (append (ptree-references x) references)))
	     (t
	      (push x nld-elements))))
	 (traverse (state)
	   (cond
	     ((state-bottom? state)
	      nil)
	     (t
	      (let ((ptree (state-ref state 0)))
		(cond
		  ((CRD? (ptree-root-label ptree))
		   (dolist (x (ptree-nld-elements ptree))
		     (when (not (eq (ptree-nld-type x) :*T*))
		       (push-nld-element x))))
		  (t
		   (dolist (x (ptree-nld-elements ptree))
		     (push-nld-element x))))
		(traverse (state-predictor state)))))))
      (traverse state)
      (nreverse (remove-if #'(lambda (x) (find (ptree-id x) references :test #'(lambda (i j) (declare (fixnum i j)) (= i j))))
			   nld-elements)))))

;; *
(declaim (inline find-SBJ-controller))
(defun find-SBJ-controller (state)
  (aif (state-c-commanding-SBJ state) (ptree-id it)))

(declaim (inline find-OBJ-controller))
(defun find-OBJ-controller (state)
  (aif (state-c-commanding-OBJ state) (ptree-id it)))

;; *T*
(defun find-*T*-filler (state e-cat)
  (let ((candidates (remove-if-not #'(lambda (x) (and (ptree-filler? x)
						      (eq (ptree-nld-type x) :*T*)
						      (eq (ptree-nld-category x) e-cat)
						      (eq (ptree-nld-direction x) :L)))
				   (state-nld-elements state))))
    (labels
	((traverse (s)
	   (cond
	     ((state-bottom? s)
	      nil)
	     (t
	      (dolist (x (ptree-subtrees (state-ref s 0)))
		(when (member x candidates :test #'eq)
		  (return-from find-*T*-filler (ptree-id x))))
	      (traverse (state-predictor s))))))
      (traverse state))))

(defun find-*T*-empty-elements (state)
  (let ((cat (remove-tags (ptree-root-label (state-ref state 1)))))
    (mapcar #'ptree-id
	    (remove-if-not #'(lambda (x)
			       (and (ptree-empty? x)
				    (eq (ptree-nld-type x) :*T*)
				    (eq (ptree-nld-direction x) :A)
				    (eq (ptree-nld-category x) cat)))
			   (ptree-nld-elements (state-ref state 0))))))

;; *EXP*
(defun find-*EXP*-NP (state)
  (labels
      ((traverse (s)
	 (cond
	   ((state-bottom? s)
	    nil)
	   (t
	    (dolist (x (ptree-subtrees (state-ptree s)))
	      (when (ptree-*EXP*-NP? x)
		(return-from find-*EXP*-NP (ptree-id x))))
	    (traverse (state-predictor s))))))
    (traverse (state-predictor state))))

;; default rule
(defun find-filler (state e-cat e-type)
  (awhen (find-if #'(lambda (x) (and (ptree-filler? x)
				     (eq (ptree-nld-category x) e-cat)
				     (eq (ptree-nld-type x) e-type)
				     (eq (ptree-nld-direction x) :L)))
		  (state-nld-elements state))
    (ptree-id it)))

(defun find-empty-element (state f-cat f-type)
  (awhen (find-if #'(lambda (x) (and (ptree-empty? x)
				     (eq (ptree-nld-category x) f-cat)
				     (eq (ptree-nld-type x) f-type)
				     (eq (ptree-nld-direction x) :R)))
		  (remove-if #'(lambda (x) (member x (ptree-nld-elements (state-ref state 0)) :test #'eq))
			     (state-nld-elements state)))

    (ptree-id it)))

(defun find-all-empty-elements (state f-cat f-type)
  (mapcar #'ptree-id
	  (remove-if-not #'(lambda (x) (and (ptree-empty? x)
					    (eq (ptree-nld-category x) f-cat)
					    (eq (ptree-nld-type x) f-type)
					    (eq (ptree-nld-direction x) :R)))
			 (remove-if-not #'(lambda (x) (member x (state-nld-elements state) :test #'eq))
					(ptree-nld-elements (state-ref state 1))))))

(defun state-get-ptree (state top)
  (cond
    ((state-final? state top)
     (state-ptree state))
    (t
     (state-intermediate-ptree state top))))

(defun state-intermediate-ptree (state top)
  (labels
      ((complete-ptrees (state ptrees)
	 (cond
	   ((state-bottom? state)
	    (reverse ptrees))
	   (t
	    (complete-ptrees (state-predictor state)
			     (cond
			       ((ptree-complete? (state-ptree state))
				(cons (state-ptree state) ptrees))
			       (t
				(revappend (ptree-subtrees (state-ptree state))
					   ptrees))))))))
    (make-ptree :root-label top
		:subtrees (complete-ptrees state
					   (map 'list
						#'(lambda (w) (make-ptree :root-label (intern "XX" (symbol-package top))
									  :complete? t
									  :word w))
						(subseq (state-sentence state) (state-position state)))))))
