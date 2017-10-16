(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defstruct node
  label
  word
  prev
  next
  parent
  leftmost)

(defmacro do-ancestors ((n node) &body body)
  `(loop :for ,n := ,node :then (node-parent ,n) :while ,n :do ,@body))

(defun tree->nodes (tree)
  (let ((nodes nil))
    (labels
	((push-node (x &optional prev parent)
	   (let ((n (make-node)))
	     (push n nodes)
	     (setf (node-label n) (root-label x)
		   (node-prev n) prev
		   (node-parent n) parent)
	     (cond
	       ((preterminal? x)
		(setf (node-word n) (terminal x)))
	       (t
		(let ((prev nil))
		  ;; leftmost
		  (setf (node-leftmost n)
			(setf prev (push-node (first (subtrees x)) nil n)))
		  ;; the others
		  (dolist (y (rest (subtrees x)))
		    (setf prev (push-node y prev n))))))

	     n)))

      (push-node tree)

      ;; set slot "next"
      (dolist (n nodes)
	(aif (node-prev n)
	     (setf (node-next it) n)))

      (nreverse nodes))))

(defun node->tree (node)
  (labels
      ((tree-list (node)
	 (if node
	     (cons (node->tree node) (tree-list (node-next node)))
	     nil)))

    (aif (node-word node)
	 (make-tree (node-label node) (list it))
	 (make-tree (node-label node) (tree-list (node-leftmost node))))))

(defun nodes->tree (nodes)
  (node->tree (first nodes)))

(defmethod label ((x node)) (node-label x))
