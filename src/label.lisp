(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defun symbol-regex-replace (regex symbol replacement)
  (intern (regex-replace regex (symbol-name symbol) replacement) (symbol-package symbol)))

(defun make-label-string (category tags)
  (apply #'str
	 category
	 (mapcar #'(lambda (x) (str "<" x ">"))
		 (sort (copy-list tags) #'string<=))))

(defmacro def-scansym (f regex)
  (with-gensyms (sym)
    `(def-memoized-function (,f (:test 'eq :key #'car)) (,sym)
       (if (scan ,regex (symbol-name ,sym)) t))))

(defmacro def-remsym (f regex)
  (with-gensyms (sym)
    `(def-memoized-function (,f (:test 'eq :key #'car)) (,sym)
       (intern (regex-replace-all ,regex (symbol-name ,sym) "")
	       (symbol-package ,sym)))))

;; tag
(def-remsym remove-tags "<[^ ]*>")

(def-scansym CRD? "<CRD>")
(def-scansym FIL? "<FIL>")
(def-scansym HD? "<HD>")
(def-scansym IDX? "<IDX>")
(def-scansym OBJCTRL? "<OBJCTRL>")
(def-scansym SBJ? "<SBJ>")

;; category
(def-scansym NP? "(^NP|<NOM>)")
(def-scansym NONE? "^-NONE-")
(def-scansym PP? "^PP")
(def-scansym PRP? "^PRP")
(def-scansym PRN? "^PRN")
(def-scansym S? "^S($|[^A-Z])")
(def-scansym VP? "^VP")

;; top and bottom features
(defun create-tag-regex (tags)
  (format nil "<(~{~a~^|~})>" tags))

(defparameter *top-features*
  '("SBJ" "HD"
    "FIL"
    "FCAT-[^<>]+"
    "FTYPE-[^<>]+"
    "FDIR-[^<>]+"
    ))

(defparameter *bottom-features*
  '("CRD"
    ))

(defparameter *top-regex* (create-tag-regex *top-features*))
(defparameter *bottom-regex* (create-tag-regex *bottom-features*))

(def-remsym remove-top-features *top-regex*)
(def-remsym remove-bottom-features *bottom-regex*)

(def-memoized-function (remove-top-and-bottom-features (:test #'eq :key #'car)) (sym)
  (remove-top-features (remove-bottom-features sym)))

;;; nonlocal dependency
(def-memoized-function (nonlocal-dependency-type (:key #'car :test 'eq)) (x)
  (cond
    ((FIL? x)
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<FTYPE-(.*)>$" tag))
	 (return-from nonlocal-dependency-type (intern (svref it 0) :keyword)))))
    (t
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<ETYPE-(.*)>$" tag))
	 (return-from nonlocal-dependency-type (intern (svref it 0) :keyword)))))))

(def-memoized-function (nonlocal-dependency-category (:key #'car :test 'eq)) (x)
  (cond
    ((FIL? x)
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<FCAT-(.*)>$" tag))
	 (return-from nonlocal-dependency-category (intern (svref it 0) (symbol-package x))))))
    (t
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<ECAT-(.*)>$" tag))
	 (return-from nonlocal-dependency-category (intern (svref it 0) (symbol-package x))))))))

(def-memoized-function (nonlocal-dependency-direction (:key #'car :test 'eq)) (x)
  (cond
    ((FIL? x)
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<FDIR-(.*)>$" tag))
	 (return-from nonlocal-dependency-direction (intern (svref it 0) :keyword)))))
    (t
     (dolist (tag (all-matches-as-strings "<[^<>]+>" (symbol-name x)))
       (awhen (nth-value 1 (scan-to-strings "^<EDIR-(.*)>$" tag))
	 (return-from nonlocal-dependency-direction (intern (svref it 0) :keyword)))))))

(defun category= (x y)
  (eq (remove-top-and-bottom-features x)
      (remove-top-and-bottom-features y)))

(def-memoized-function (extract-top-features (:key #'car :test 'eq)) (x)
  (let ((features nil))
    (dolist (tag *top-features*)
      (when (scan (str "<" tag ">") (symbol-name x))
	(push tag features)))
    features))

(def-memoized-function (extract-bottom-features (:key #'car :test 'eq)) (x)
  (let ((features nil))
    (dolist (tag *bottom-features*)
      (when (scan (str "<" tag ">") (symbol-name x))
	(push tag features)))
    features))

(def-memoized-function (unify-label (:key #'list*)) (top bottom)
  (cond
    ((category= top bottom)
     (let ((category (regex-replace-all  "<[^ ]*>" (symbol-name top) ""))
	   (annotations (append (mapcar #'(lambda (tag) (regex-replace-all "[<>]" tag ""))
					(all-matches-as-strings "<[^ <>]+>" (symbol-name (remove-bottom-features top))))
				(extract-bottom-features bottom))))
       (intern (make-label-string category annotations) (symbol-package top))))
    (t
     nil)))
