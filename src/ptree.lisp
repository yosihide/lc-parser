(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

;;; head
(defstruct head
  pos
  word
  )

(defstruct (ptree (:constructor make-ptree%))
  root-label
  subtrees
  word

  (id 0 :type fixnum)
  (last-id 0 :type fixnum)

  (complete? nil :type boolean)

  ;; nonlocal dependency
  references

  (filler? nil :type boolean)
  nld-category
  nld-type
  nld-direction
  nld-elements

  ;; head information
  head-subtree
  head
  )

(declaim (inline ptree-hpos ptree-hword))
(defun ptree-hpos (ptree)
  (awhen (ptree-head ptree)
    (head-pos it)))

(defun ptree-hword (ptree)
  (awhen (ptree-head ptree)
    (head-word it)))

(declaim (inline ptree-empty?))
(defun ptree-empty? (ptree)
  (not (ptree-filler? ptree)))

(defun make-ptree (&key
		   root-label
		   subtrees
		   word
		   complete?
		   (id 0)
		   references
		   )
  (declare (fixnum id))

  (let ((ptree nil)
	(head-subtree (find-if #'HD? subtrees :key #'ptree-root-label))
	(nld-elements nil))

    (setf ptree
	  (make-ptree% :root-label root-label
		       :subtrees subtrees
		       :word word

		       :complete? complete?

		       :id id
		       :last-id (cond
				  ((null subtrees)
				   id)
				  (t
				   (max id (ptree-last-id (first subtrees)))))

		       :references references

		       :head-subtree head-subtree
		       :head (acond
			      ((null subtrees)
			       (make-head :pos root-label :word (word-feature word)))
			      ((and head-subtree (ptree-head head-subtree))
			       it))))

    ;; nonlocal dependency elements
    (cond
      ((not complete?)
       nil)

      ;; empty element
      ((IDX? root-label)
       (setf (ptree-nld-category ptree) (nonlocal-dependency-category root-label))
       (setf (ptree-nld-type ptree) (nonlocal-dependency-type root-label))
       (setf (ptree-nld-direction ptree) (nonlocal-dependency-direction root-label))
       (push ptree nld-elements))

      ;; dislocation filler (X<FIL> ...)
      ((FIL? root-label)
       (setf (ptree-filler? ptree) t)
       (setf (ptree-nld-category ptree) (nonlocal-dependency-category root-label))
       (setf (ptree-nld-type ptree) (nonlocal-dependency-type root-label))
       (setf (ptree-nld-direction ptree) (nonlocal-dependency-direction root-label))
       (push ptree nld-elements))

      ;; filler with no <FIL> tag
      (references
       (setf (ptree-filler? ptree) t)
       (push ptree nld-elements)))

    ;; collect nonlocal dependency elements
    (dolist (x subtrees)
      (dolist (y (ptree-nld-elements x))
	(push y nld-elements)))
    (let ((refs nil)
	  (indexes nil))
      (dolist (x nld-elements)
	(setf refs (append (ptree-references x) refs))
	(push (ptree-id x) indexes))
      (setf (ptree-nld-elements ptree)
	    (nreverse (remove-if #'(lambda (x) (or (find (ptree-id x) refs :test #'(lambda (i j) (declare (fixnum i j)) (= i j)))
						   (some #'(lambda (i) (find i indexes :test #'(lambda (i j) (declare (fixnum i j)) (= i j))))
							 (ptree-references x))))
				 nld-elements))))

    (return-from make-ptree ptree)))

(declaim (inline ptree-preterminal?))
(defun ptree-preterminal? (ptree)
  (null (ptree-subtrees ptree)))

;; action
(declaim (inline ptree-remove-top-features))
(defun ptree-remove-top-features (ptree)
  (make-ptree :root-label (remove-top-features (ptree-root-label ptree))
	      :subtrees (ptree-subtrees ptree)
	      :word (ptree-word ptree)
	      :complete? (ptree-complete? ptree)
	      :id (ptree-id ptree)
	      :references (ptree-references ptree)))

;; LeftCorner
(declaim (inline make-left-corner-rule left-corner-rule-parent left-corner-rule-child left-corner-rule-complete))
(defun make-left-corner-rule (parent child complete) (list parent child complete))
(defun left-corner-rule-parent (rule) (nth 0 rule))
(defun left-corner-rule-child (rule) (nth 1 rule))
(defun left-corner-rule-complete (rule) (nth 2 rule))

(declaim (inline ptree-left-corner))
(defun ptree-left-corner (ptree rule)
  (let ((child-label (left-corner-rule-child rule))
	(new-subtree nil))
    (cond
      ((eq child-label (ptree-root-label ptree))
       (setf new-subtree ptree))
      (t
       (setf new-subtree (make-ptree :root-label (unify-label child-label (ptree-root-label ptree))
				     :subtrees (ptree-subtrees ptree)
				     :word (ptree-word ptree)
				     :complete? (ptree-complete? ptree)
				     :id (ptree-id ptree)
				     :references (ptree-references ptree)))))
    (make-ptree :root-label (left-corner-rule-parent rule)
		:subtrees (list new-subtree)
		:complete? (left-corner-rule-complete rule)
		:id (1+ (ptree-last-id ptree)))))

;; Attach
(declaim (inline make-attach-rule attach-rule-label attach-rule-complete))
(defun make-attach-rule (label complete) (list label complete))
(defun attach-rule-label (rule) (nth 0 rule))
(defun attach-rule-complete (rule) (nth 1 rule))

(declaim (inline ptree-attach))
(defun ptree-attach (ptree1 ptree0 rule &key right-references root-references)
  (let ((label (attach-rule-label rule))
	(new-ptree0 nil))
    (cond
      ((not (eq label (ptree-root-label ptree0)))
       (setf new-ptree0
	     (make-ptree :root-label (unify-label label (ptree-root-label ptree0))
			 :subtrees (ptree-subtrees ptree0)
			 :word (ptree-word ptree0)
			 :complete? (ptree-complete? ptree0)
			 :id (ptree-id ptree0)
			 :references right-references)))
      (right-references
       (setf new-ptree0 (copy-ptree ptree0))
       (setf (ptree-references new-ptree0) right-references))
      (t
       (setf new-ptree0 ptree0)))
    (make-ptree :root-label (ptree-root-label ptree1)
		:subtrees (cons new-ptree0 (ptree-subtrees ptree1))
		:complete? (attach-rule-complete rule)
		:id (ptree-id ptree1)
		:references (append root-references (ptree-references ptree1)))))


;; nonlocal dependency identification
;; *
(declaim (inline ptree-SBJ?))
(defun ptree-SBJ? (ptree)
  (SBJ? (ptree-root-label ptree)))

(defun ptree-c-commanding-SBJ (ptree)
  (dolist (x (ptree-subtrees ptree))
    (when (ptree-SBJ? x)
      (return-from ptree-c-commanding-SBJ x))))

(defun ptree-c-commanding-OBJ (ptree)
  (when (VP? (ptree-root-label ptree))
    (dolist (x (ptree-subtrees ptree))
      (when (NP? (ptree-root-label x))
	(return-from ptree-c-commanding-OBJ x))
      (awhen (and (PP? (ptree-root-label x))
		  (find-if #'NP? (ptree-subtrees x) :key #'ptree-root-label))
	(return-from ptree-c-commanding-OBJ it)))))

;; *EXP*
(declaim (inline ptree-*EXP*-NP?))
(defun ptree-*EXP*-NP? (ptree)
  (and (NP? (ptree-root-label ptree))
       (single? (ptree-subtrees ptree))
       (PRP? (ptree-root-label (ptree-head-subtree ptree)))
       (string-equal (word-occurrence (ptree-word (ptree-head-subtree ptree))) "it")))

(defun ptree->tree (ptree)
  (cond
    ((null (ptree-subtrees ptree))
     (make-tree (ptree-root-label ptree)
		(aif (ptree-word ptree) (list (word-occurrence it)))))
    (t
     (make-tree (intern (str (symbol-name (ptree-root-label ptree)) "-" (format nil "~d" (ptree-id ptree)))
			(symbol-package (ptree-root-label ptree)))
		(nreverse (mapcar #'ptree->tree (ptree-subtrees ptree)))))))

(defun tree->ptree (tree)
  (cond
    ((preterminal? tree)
     (make-ptree :root-label (root-label tree)
		 :word (make-word :occurrence (terminal tree))
		 :complete? t))
    (t
     (make-ptree :root-label (root-label tree)
		 :subtrees (nreverse (mapcar #'tree->ptree (subtrees tree)))
		 :complete? t))))

;; recover nonlocal dependency
(defun empty-element-id=>filler-id (ptree)
  (let ((empty-element-id=>filler-id (make-hash-table)))
    (labels
	((traverse (ptree)
	   (cond
	     ((ptree-preterminal? ptree)
	      nil)
	     (t
	      (dolist (empty-element-id (ptree-references ptree))
		(setf (gethash empty-element-id empty-element-id=>filler-id) (ptree-id ptree)))
	      (dolist (x (ptree-subtrees ptree))
		(traverse x))))))
      (traverse ptree))
    empty-element-id=>filler-id))

(defun id=>*EXP*-filler (ptree)
  (let ((id=>*EXP*-filler (make-hash-table)))
    (labels
	((traverse (ptree)
	   (cond
	     ((ptree-preterminal? ptree)
	      nil)
	     (t
	      (when (eq (nonlocal-dependency-type (ptree-root-label ptree)) :*EXP*)
		(dolist (id (ptree-references ptree))
		  (setf (gethash id id=>*EXP*-filler) ptree)))
	      (dolist (x (ptree-subtrees ptree))
		(traverse x))))))
      (traverse ptree))
    id=>*EXP*-filler))

(defun make-empty-terminal (type id)
  (cond
    (id
     (str (symbol-name type) "-" (format nil "~d" id)))
    (t
     (symbol-name type))))

(defun make-*EXP*-ptree (ptree f-cat id)
  (make-ptree :root-label (ptree-root-label ptree)
	      :subtrees (list (make-ptree :root-label f-cat
					  :subtrees (list (make-ptree :root-label (intern "-NONE-" (symbol-package (ptree-root-label ptree)))
								      :word (make-word :occurrence (make-empty-terminal :*EXP* id))
								      :complete? t))
					  :complete? t)
			      (make-ptree :root-label (remove-top-features (ptree-root-label ptree))
					  :subtrees (ptree-subtrees ptree)
					  :complete? t))
	      :complete? t))

(defun ptree-recover-nonlocal-dependency (ptree)
  (let ((empty-element-id=>filler-id (empty-element-id=>filler-id ptree))
	(id=>*EXP*-filler (id=>*EXP*-filler ptree)))
    (labels
	((aux (ptree)
	   (acond
	     ((ptree-preterminal? ptree)
	      (acond
	       ((gethash (ptree-id ptree) empty-element-id=>filler-id)
		(make-ptree :root-label (ptree-root-label ptree)
			    :complete? t
			    :id (ptree-id ptree)
			    :references (ptree-references ptree)
			    :word (make-word :occurrence
					     (make-empty-terminal (nonlocal-dependency-type (ptree-root-label ptree)) it))))
	       ((NONE? (ptree-root-label ptree))
		(make-ptree :root-label (ptree-root-label ptree)
			    :complete? t
			    :id (ptree-id ptree)
			    :references (ptree-references ptree)
			    :word (make-word :occurrence
					     (make-empty-terminal (nonlocal-dependency-type (ptree-root-label ptree))
								  (first (ptree-references ptree))))))
	       (t
		ptree)))

	     ((gethash (ptree-id ptree) id=>*EXP*-filler)
	      (make-*EXP*-ptree ptree (ptree-nld-category it) (ptree-id it)))
	     (t
	      (make-ptree :root-label (ptree-root-label ptree)
			  :subtrees (mapcar #'aux (ptree-subtrees ptree))
			  :complete? (ptree-complete? ptree)
			  :id (ptree-id ptree)
			  :references (ptree-references ptree))))))
      (aux ptree))))
