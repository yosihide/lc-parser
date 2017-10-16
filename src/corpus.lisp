(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

;;; Utilities
;; category
(declaim (inline category))
(defun category (label)
  (alist-get :category label))

(declaim (inline category-match?))
(defun category-match? (x &rest categories)
  (let ((c (category (label x))))
    (if (member c categories :test #'string=) t)))

;; function tag
(defun functions (label)
  (alist-get :functions label))

(defun function? (tag tree)
  (member tag (functions (label tree)) :test #'equal))

;; annotation
(defun annotations (label)
  (alist-get :annotations label))

(defun annotation? (tag tree)
  (member tag (annotations (label tree)) :test #'equal))

(defun push-annotation (tag label)
  (alist-push :annotations tag label
	      :test #'eq))

(defun pushnew-annotation (tag label)
  (alist-pushnew :annotations tag label
		 :key-test #'eq
		 :value-test #'equal))

;; index
(defun -index (label)
  (alist-get :-index label))

;;; nonlocal dependency
;;; empty element
(defun empty-element? (node)
  (aand (node-leftmost node)
	(and (null (node-next it))
	     (category-match? it "-NONE-"))))

(defun indexed? (terminal)
  (scan "-[0-9]+$" terminal))

(defun remove-index (terminal)
  (regex-replace "-[0-9]+$" terminal ""))

(defun get-index (terminal)
  (read-from-string (regex-replace-all "[^0-9]" terminal "")))

(defun get-e-index (node)
  (if (and (empty-element? node)
	   (indexed? (node-word (node-leftmost node))))
      (get-index (node-word (node-leftmost node)))))

(defun get-e-type (node)
  (if (empty-element? node)
      (remove-index (node-word (node-leftmost node)))))

;;; filler
(defun get-f-index (x)
  (-index (label x)))

;;; Coordinated Structure
(defun conjunct-candidate? (tree parent)
  (and (not (annotation? "FIL" tree))
       (or (category-match? tree parent "FRAG")
	   (and (equal parent "S")
		(category-match? tree "SINV" "SQ"))
	   (and (equal parent "UCP")
		(not (preterminal? tree))
		(not (category-match? tree "CONJP")))
	   (every #'equalp '("so" "on") (terminals tree)))))

(defun coordinated? (tree)
  (cond
    ((preterminal? tree)
     nil)
    (t
     (loop
	:for i :of-type fixnum :from 0 :below (length (subtrees tree))
	:with cat := (category (root-label tree))
	:thereis
	(and (category-match? (tree-ref tree i) "CC" "CONJP" ":" ",")
	     (find-if #'(lambda (x) (conjunct-candidate? x cat)) (subtrees tree) :end i)
	     (find-if #'(lambda (x) (conjunct-candidate? x cat)) (subtrees tree) :start (1+ i)))))))

(defun annotate-CRD (tree)
  (cond
    ((preterminal? tree)
     tree)
    ((coordinated? tree)
     (make-tree (pushnew-annotation "CRD" (root-label tree))
		(mapcar #'annotate-CRD (subtrees tree))))
    (t
     (make-tree (root-label tree)
		(mapcar #'annotate-CRD (subtrees tree))))))

;; The head rules of (Surdeanu et al., CoNLL2008)
;; the second elements represent a direction ( t means "from-end" .).
(defparameter *head-rules*
  '((("ADJP") t "NNS" "QP" "NN" "\\$" "ADVP" "JJ" "VBN" "VBG" "ADJP" "JJR" "NP" "JJS" "DT" "FW" "RBR" "RBS" "SBAR" "RB" :*)
    (("ADVP") nil "RB" "RBR" "RBS" "FW" "ADVP" "TO" "CD" "JJR" "JJ" "IN" "NP" "JJS" "NN" :*)
    (("CONJP") nil "CC" "RB" "IN" :*)
    (("FRAG") nil "(NN.*|NP)" "W.*" "SBAR" "(PP|IN)" "(ADJP|JJ)" "ADVP" "RB" :*)
    (("INTJ") t :*)
    (("LST") nil "LS" ":" :*)
    (("NAC" "NP" "NX" "WHNP") t "(NN.*|NX)" :NP "JJR" "CD" "JJ" "JJS" "RB" "QP" "NP" :*)
    (("PP" "WHPP") nil "IN" "TO" "VBG" "VBN" "RP" "FW" :*)
    (("PRN") nil "S.*" "N.*" "W.*" "(PP|IN)" "(ADJP|JJ.*)" "(ADVP|RB.*)" :*)
    (("PRT") nil "RP" :*)
    (("QP") t "\\$" "IN" "NNS" "NN" "JJ" "RB" "DT" "CD" "NCD" "QP" "JJR" "JJS" :*)
    (("RRC") nil "VP" "NP" "ADVP" "ADJP" "PP" :*)
    (("S") t "VP" :PRD "S" "SBAR" "ADJP" "UCP" "NP" :*)
    (("SBAR") t "S" "SQ" "SINV" "SBAR" "FRAG" "IN" "DT" :*)
    (("SBARQ") t "SQ" "S" "SINV" "SBARQ" "FRAG" :*)
    (("SINV") t "VBZ" "VBD" "VBP" "VB" "MD" "VP" :PRD "S" "SINV" "ADJP" "NP" :*)
    (("SQ") t "VBZ" "VBD" "VBP" "VB" "MD" :PRD "VP" "SQ" :*)
    (("UCP") nil :*)
    (("VP") nil "VBD" "VBN" "MD" "VBZ" "VB" "VBG" "VBP" "VP" :PRD "ADJP" "NN" "NNS" "NP" :*)
    (("WHADJP") t "CC" "WRB" "JJ" "ADJP" :*)
    (("WHADVP") nil "CC" "WRB" :*)
    (("X") nil :*)

    (("PRT|ADVP") nil "RP" :*)
    (("TOP") nil :*)
    ))

(defun create-finder (obj)
  (cond
    ((eq obj :NP)
     #'(lambda (tree)
	 (and (category-match? tree "NP")
	      (null (functions (label tree))))))
    ((eq obj :PRD)
     #'(lambda (tree)
	 (function? "PRD" tree)))
    ((eq obj :*)
     #'(lambda (tree)
	 (declare (ignore tree))
	 t))
    (t
     (let ((regex (str "^" obj "$")))
       #'(lambda (tree)
	   (scan regex (category (root-label tree))))))))

(defun create-head-table (head-rules)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (rule head-rules)
      (let ((direction (second rule))
	    (priority-list (mapcar #'create-finder (nthcdr 2 rule))))
	(dolist (cat (first rule))
	  (setf (gethash cat table)
		(cons direction priority-list)))))
    table))

(defparameter *head-table*
  (create-head-table *head-rules*))

(defun head-child-position (tree)
  (cond
    ((coordinated? tree)
     (position-if #'(lambda (x) (conjunct-candidate? x (category (root-label tree))))
		  (subtrees tree)))
    (t
     (let* ((rule (gethash (category (root-label tree)) *head-table*))
	    (direction (first rule))
	    (priority-list (rest rule)))
       (dolist (finder priority-list)
	 (awhen (position-if finder (subtrees tree) :from-end direction)
	   (return-from head-child-position it)))))))

(defun annotate-head (tree &optional head?)
  (let ((root-label (root-label tree)))
    (when head?
      (setf root-label
	    (pushnew-annotation "HD" root-label)))

    (cond
      ((preterminal? tree)
       (make-tree root-label (subtrees tree)))

      (t
       (let ((subtrees nil)
	     (head-position (or (head-child-position tree) -1)))
	 (declare (fixnum head-position))

	 (dotimes (i (length (subtrees tree)))
	   (push (annotate-head (nth i (subtrees tree)) (= i head-position))
		 subtrees))
	 (make-tree root-label (nreverse subtrees)))))))

(defun label-parsed-tree (tree)
  (cond
    ((preterminal? tree)
     (make-tree (parse-label (root-label tree))
		(subtrees tree)))
    (t
     (make-tree (parse-label (root-label tree))
		(mapcar #'label-parsed-tree (subtrees tree))))))

(defun annotate-SBJ (nodes)
  (dolist (n nodes)
    (when (function? "SBJ" n)
      (setf (node-label n)
	    (pushnew-annotation "SBJ" (node-label n)))))
  nodes)

(defun annotate-NOM (nodes)
  (dolist (n nodes)
    (when (function? "NOM" n)
      (setf (node-label n)
	    (pushnew-annotation "NOM" (node-label n)))))
  nodes)

(defun annotate-nonlocal-dependency (nodes)
  (let ((index=>empty-element (make-hash-table))
	(index=>filler (make-hash-table)))

    ;; extract empty elements and fillers
    (dolist (n nodes)
      ;; empty element
      (let ((e-index (get-e-index n)))
	(when (not (suc-find-if #'node-parent #'(lambda (x) (eql (get-f-index x) e-index)) n))
	  (setf (gethash e-index index=>empty-element) n)))

      ;; filler
      (awhen (get-f-index n)
	(setf (gethash it index=>filler) n)))

    ;; assign type and category to fillers
    (dolist (n nodes)
      (awhen (gethash (get-f-index n) index=>empty-element)
	(when (not (equal (get-e-type it) "*"))
	  (setf (node-label n) (pushnew-annotation "FIL" (node-label n)))
	  (setf (node-label n) (pushnew-annotation (str "FTYPE-" (get-e-type it)) (node-label n)))
	  (setf (node-label n) (pushnew-annotation (str "FCAT-" (category (node-label it))) (node-label n))))))

    (let ((filler-index-table (make-hash-table)))
      (dolist (n nodes)
	(acond
	 ((get-f-index n)
	  (setf (gethash it filler-index-table) t))
	 ((category-match? n "-NONE-")

	  ;; assign nonlocal dependency type to empty elements
	  (setf (node-label n)
		(pushnew-annotation (str "ETYPE-" (remove-index (node-word n))) (node-label n)))

	  (awhen (indexed? (node-word n))

	    (setf (node-label n)
		  (pushnew-annotation "IDX" (node-label n)))

	    (let ((index (get-index (node-word n)))
		  (type (remove-index (node-word n))))
	      ;; assign <OBJCTRL>
	      (when (and (equal type "*")
			 (not (annotation? "SBJ" (gethash index index=>filler))))
		(setf (node-label n)
		      (pushnew-annotation "OBJCTRL" (node-label n))))

	      ;; assign empty element category
	      (when (not (equal type "*"))
		(setf (node-label n)
		      (pushnew-annotation (str "ECAT-" (category (node-label (node-parent n)))) (node-label n))))

	      ;; nonlocal dependency direction
	      (cond
		;; ancestor
		((null (gethash index index=>empty-element))
		 (setf (node-label n) (pushnew-annotation "EDIR-A" (node-label n))))

		;; left
		((gethash index filler-index-table)
		 (awhen (gethash index index=>filler)
		   (when (not (equal (remove-index (node-word n)) "*"))
		     (setf (node-label it) (pushnew-annotation "FDIR-L" (node-label it)))))
		 (setf (node-label n) (pushnew-annotation "EDIR-L" (node-label n))))

		;; right
		(t
		 (awhen (gethash index index=>filler)
		   (when (not (equal (remove-index (node-word n)) "*"))
		     (setf (node-label it) (pushnew-annotation "FDIR-R" (node-label it)))))
		 (setf (node-label n) (pushnew-annotation "EDIR-R" (node-label n)))))))

	  ;; replace the terminal of empty element with 0
	  (setf (node-word n) 0))))))
  nodes)

(defun remove-NONE (tree)
  (remove-nodes-if #'(lambda (x) (string= (category (root-label x)) "-NONE-")) tree))

(defun remove-*EXP* (tree)
  (remove-nodes-if #'(lambda (x) (and (string= (category (root-label x)) "-NONE-")
				      (annotation? "ETYPE-*EXP*" x)))
		   tree))

;;; remove recursive unary rules
(defun remove-recursive-unary-rule (tree)
  (cond
    ((preterminal? tree)
     tree)
    ((and (= (length (subtrees tree)) 1)
	  (string= (category (root-label tree))
		   (category (root-label (first (subtrees tree))))))
     (remove-recursive-unary-rule (make-tree (root-label tree)
					     (subtrees (first (subtrees tree))))))
    (t
     (make-tree (root-label tree)
		(mapcar #'remove-recursive-unary-rule (subtrees tree))))))

(defun make-label (label)
  (intern (make-label-string (category label) (annotations label))))

(defun annotated-tree (tree)
  (let ((root-label (make-label (root-label tree))))
    (cond
      ((preterminal? tree)
       (make-tree root-label (subtrees tree)))
      (t
       (make-tree root-label
		  (mapcar #'annotated-tree (subtrees tree)))))))

(defun make-corpus (input-trees &key (nld t))
  (let ((trees nil))
    (dolist (tree input-trees)
      (let ((nodes (tree->nodes (label-parsed-tree tree))))

	(setf nodes (annotate-NOM nodes))

	(when nld
	  (setf nodes (annotate-SBJ nodes)))

	(when nld
	  (setf nodes (annotate-nonlocal-dependency nodes)))

	(setf tree (nodes->tree nodes))

	(unless nld
	  (setf tree (remove-NONE tree)))

	(setf tree (remove-*EXP* tree))

	(setf tree (remove-recursive-unary-rule tree))
	(setf tree (annotate-head tree))

	(when nld
	  (setf tree (annotate-CRD tree)))

	(setf tree (annotated-tree tree))
	(push tree trees)))
    (nreverse trees)))

(defun make-cf-corpus (input-trees)
  (let ((trees nil))
    (dolist (tree input-trees)
      (setf tree (label-parsed-tree tree))
      (setf tree (remove-NONE tree))
      (setf tree (annotated-tree tree))
      (push tree trees))
    (nreverse trees)))

(defun remove-nld-annotations (tree)
  (labels
      ((get-index-table (tree)
	 (let ((table (make-hash-table :test 'equal)))
	   (labels
	       ((get-index-table-aux (tree)
		  (cond
		    ((preterminal? tree)
		     (awhen (aand (NONE? (root-label tree))
				  (nth 1 (split "-" (terminal tree)))
				  (read-from-string it))
		       (setf (gethash it table) it)))
		    (t
		     (dolist (x (subtrees tree))
		       (get-index-table-aux x))))))
	     (get-index-table-aux tree))
	   table)))
    (let* ((table (get-index-table tree)))
      (labels
	  ((aux (tree)
	     (let ((root-label (remove-tags (root-label tree))))
	       (cond
		 ((preterminal? tree)
		  (make-tree root-label (subtrees tree)))
		 (t
		  (unless (gethash (alist-get :-index (parse-label (root-label tree))) table)
		    (setf root-label
			  (intern (alist-get :category (parse-label root-label)) (symbol-package root-label))))
		  (make-tree root-label
			     (mapcar #'aux (subtrees tree))))))))
	(aux tree)))))

(defun write-tokenized-sentences (trees output)
  (with-open-file (out output :direction :output)
    (dolist (tree trees)
      (format out "~{~a~^ ~}~%" (sentence tree)))
    t))

(defun read-tokenized-sentences (input)
  (with-open-file (in input)
    (do ((line)
	 (sentences))
	((eq (setf line (read-line in nil nil)) nil) (nreverse sentences))
      (push (split "\\s+" (regex-replace-all "(^\\s+|\\s+$)" line "")) sentences))))
