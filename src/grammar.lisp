(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defstruct grammar
  top
  nld-feature
  (minsup 0 :type fixnum)
  (e-max 0 :type fixnum)
  (ratio 0.0 :type single-float)
  e-shift-table
  left-corner-table
  attach-table
  word-table
  word-pos-table
  )

(defun extract-tuples (tree)
  (let ((state (make-state))
	(e-shift nil)
	(left-corner nil)
	(attach nil))
    (dolist (a (actions tree))
      (case (action-name a)

	(:shift
	 (when (NONE? (action-argument a))
	   (push (cons a (state-e-shift-main-context state))
		 e-shift)))
	(:left-corner
	 (push (cons a (state-left-corner-main-context state))
	       left-corner))
	(:attach
	 (push (cons a (state-attach-main-context state))
	       attach)))

      (setf state (apply-action a state)))

    (list e-shift left-corner attach)))

(defun extract-grammar (corpus minsup nld-feature)
  (declare (fixnum minsup))

  (let ((e-shift-counter (make-counter :test 'equal))
	(e-shift-table (make-hash-table :test 'equal))
	(left-corner-counter (make-counter :test 'equal))
	(left-corner-table (make-hash-table :test 'equal))
	(attach-counter (make-counter :test 'equal))
	(attach-table (make-hash-table :test 'equal))
	(word-counter (make-counter :test 'equal))
	(word-table (make-hash-table :test 'equal))
	(word-pos-counter (make-counter :test 'equal))
	(word-pos-table (make-hash-table :test 'equal))
	(e-max 0)
	(ratio 0.0))
    (declare (fixnum e-max)
	     (single-float ratio))

    (dolist (tree corpus)
      (dolist (w (sentence tree))
	(dolist (s (word-suffixes w))
	  (counter-inc s 1 word-counter))))

    (dolist (tree corpus)
      (setf ratio
	    (max ratio (/ (float (length (actions tree))) (length (sentence tree))))))

    (dolist (tree corpus)
      (setf e-max
	    (apply #'max (cons e-max (mapcar #'length (sublist (preterminal-trees tree) #'(lambda (x) (NONE? (root-label x)))))))))

    (dolist (tree corpus)
      (destructuring-bind (e-shift left-corner attach) (extract-tuples tree)

	(dolist (tuple e-shift)
	  (counter-inc tuple 1 e-shift-counter))

	(dolist (tuple left-corner)
	  (counter-inc tuple 1 left-corner-counter))

	(dolist (tuple attach)
	  (counter-inc tuple 1 attach-counter)))

      (dolist (x (preterminal-trees tree))
	(cond
	  ((NONE? (root-label x))
	   (counter-inc (cons 0 (remove-top-features (label x))) 1 word-pos-counter))
	  (t
	   (dolist (s (word-suffixes (terminal x)))
	     (counter-inc (cons s (remove-top-features (label x))) 1 word-pos-counter))))))

    (dolist (tuple (counter-frequent-elements e-shift-counter 1))
      (destructuring-bind (action . context) tuple
	(push action (gethash context e-shift-table))))

    (dolist (tuple (counter-frequent-elements left-corner-counter 1))
      (destructuring-bind (action . context) tuple
	(push action (gethash context left-corner-table))))

    (dolist (tuple (counter-frequent-elements attach-counter 1))
      (destructuring-bind (action . context) tuple
	(push action (gethash context attach-table))))

    (dolist (w (counter-elements word-counter))
      (setf (gethash w word-table) (counter-count w word-counter)))

    (dolist (pair (counter-elements word-pos-counter))
      (push (cdr pair) (gethash (car pair) word-pos-table)))

    (make-grammar :top (root-label (first corpus))
		  :nld-feature nld-feature
		  :minsup minsup
		  :ratio ratio
		  :e-max e-max
		  :e-shift-table e-shift-table
		  :left-corner-table left-corner-table
		  :attach-table attach-table
		  :word-table word-table
		  :word-pos-table word-pos-table)))

(defun grammar-save (grammar file)
  (let ((*print-pretty* nil))
    (write-objects (append (list (cons :top (grammar-top grammar)))
			   (list (cons :nld-feature (grammar-nld-feature grammar)))
			   (list (cons :minsup (grammar-minsup grammar)))
			   (list (cons :e-max (grammar-e-max grammar)))
			   (list (cons :ratio (grammar-ratio grammar)))
			   (table->entries :e-shift (grammar-e-shift-table grammar))
			   (table->entries :left-corner (grammar-left-corner-table grammar))
			   (table->entries :attach (grammar-attach-table grammar))
			   (table->entries :word (grammar-word-table grammar))
			   (table->entries :word-pos (grammar-word-pos-table grammar)))
		   file)))

(defun grammar-load (file)
  (let ((objects (read-objects file))
	(top nil)
	(nld-feature nil)
	(minsup nil)
	(e-max nil)
	(ratio nil)
	(e-shift-table (make-hash-table :test 'equal))
	(left-corner-table (make-hash-table :test 'equal))
	(attach-table (make-hash-table :test 'equal))
	(word-table (make-hash-table :test 'equal))
	(word-pos-table (make-hash-table :test 'equal)))
    (dolist (obj objects)
      (case (car obj)
	(:top
	 (setf top (cdr obj)))
	(:nld-feature
	 (setf nld-feature (cdr obj)))
	(:minsup
	 (setf minsup (cdr obj)))
	(:e-max
	 (setf e-max (cdr obj)))
	(:ratio
	 (setf ratio (cdr obj)))
	(:e-shift
	 (setf (gethash (cadr obj) e-shift-table) (cddr obj)))
	(:left-corner
	 (setf (gethash (cadr obj) left-corner-table) (cddr obj)))
	(:attach
	 (setf (gethash (cadr obj) attach-table) (cddr obj)))
	(:word
	 (setf (gethash (cadr obj) word-table) (cddr obj)))
	(:word-pos
	 (setf (gethash (cadr obj) word-pos-table) (cddr obj)))))
    (make-grammar :top top
		  :nld-feature nld-feature
		  :minsup minsup
		  :e-max e-max
		  :ratio ratio
		  :e-shift-table e-shift-table
		  :left-corner-table left-corner-table
		  :attach-table attach-table
		  :word-table word-table
		  :word-pos-table word-pos-table)))

(defun state-left-corner-actions (state grammar)
  (gethash (state-left-corner-main-context state)
	   (grammar-left-corner-table grammar)))

(defun state-attach-actions (state grammar)
  (gethash (state-attach-main-context state)
	   (grammar-attach-table grammar)))

(defun state-e-shift-actions (state grammar)
  (gethash (state-e-shift-main-context state)
	   (grammar-e-shift-table grammar)))

;;; word
;; Rare word representation (Hall et al., ACL2014)
(defun frequent-suffix (word-string grammar)
  (dolist (s (word-suffixes word-string))
    (when (> (the fixnum (gethash s (grammar-word-table grammar) 0)) (grammar-minsup grammar))
      (return-from frequent-suffix s))))

(defun get-pos-tags (suffix grammar)
  (gethash suffix (grammar-word-pos-table grammar)))

(defun analyze-word (word &optional grammar)
  (cond
    ((and grammar (stringp word))
     (let ((suffix (frequent-suffix word grammar)))
       (make-word :occurrence word
		  :initial (word-string-initial word)
		  :suffix suffix
		  :pos-tags (get-pos-tags suffix grammar)
		  :feature (cond
			     ((eq (first suffix) :suffix)
			      :useless)
			     (t
			      suffix)))))
    (t
     (make-word :occurrence word
		:initial nil
		:suffix nil
		:feature nil))))

(defun allowable-actions (state grammar)
  (let ((actions nil))
    ;; idle
    (when (state-final? state (grammar-top grammar))
      (push (make-action :idle nil) actions))

    ;; left-corner
    (dolist (action (state-left-corner-actions state grammar))
      (push action actions))

    ;; attach
    (when (and (state-ref state 0)
	       (state-ref state 1))
      (dolist (action (state-attach-actions state grammar))
	(let ((rule (action-argument action)))
	  (cond
	    ((and (s1.h.c state)
		  (HD? (attach-rule-label rule)))
	     nil)
	    ((and (attach-rule-complete rule)
		  (null (s1.h.c state))
		  (not (HD? (attach-rule-label rule))))
	     nil)
	    (t
	     (push action actions))))))

    ;; shift
    (when (and (or (state-bottom? state)
		   (not (ptree-complete? (state-ref state 0))))
	       (< (state-position state) (length (state-sentence state))))

      (when (< (state-e-num state) (grammar-e-max grammar))
	(dolist (action (state-e-shift-actions state grammar))
	  (push action actions)))

      (dolist (label (word-pos-tags (state-nth-word (state-position state) state)))
	(push (make-action :shift label) actions)))

    actions))
