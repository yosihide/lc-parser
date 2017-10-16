(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

;;; features
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accessor (a-name)
    (intern (symbol-name a-name)))

  (defun template (a-names)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name a-names)) :keyword))

  (defmacro extract-context-feature (state &rest a-names)
    (with-gensyms (g-state)
      `(let ((,g-state ,state))
	 (make-context-feature ,(template a-names) (list ,@(mapcar #'(lambda (a-name) `(,(accessor a-name) ,g-state)) a-names)))))))

;;; Accessor
;; s0
(declaim (inline s0.c s0.t s0.w))
(defun s0.c (state)
  (aand (state-ref state 0)
	(cons (ptree-root-label it) (ptree-complete? it))))

(defun s0.t (state)
  (aand (state-ref state 0)
	(ptree-hpos it)))

(defun s0.w (state)
  (aand (state-ref state 0)
	(ptree-hword it)))

(declaim (inline s0.r.c s0.r.w))
(defun s0.r.c (state)
  (aand (state-ref state 0)
	(first (ptree-subtrees it))
	(ptree-root-label it)))

(defun s0.r.w (state)
  (aand (state-ref state 0)
	(first (ptree-subtrees it))
	(ptree-hword it)))

(declaim (inline s0.l.c s0.l.w))
(defun s0.l.c (state)
  (aand (state-ref state 0)
	(last1 (ptree-subtrees it))
	(ptree-root-label it)))

(defun s0.l.w (state)
  (aand (state-ref state 0)
	(last1 (ptree-subtrees it))
	(ptree-hword it)))

(declaim (inline s0.h.c))
(defun s0.h.c (state)
  (aand (state-ref state 0)
	(ptree-head-subtree it)
	(ptree-root-label it)))

;; s1
(declaim (inline s1.c s1.t s1.w))
(defun s1.c (state)
  (aand (state-ref state 1)
	(cons (ptree-root-label it) (ptree-complete? it))))

(defun s1.t (state)
  (aand (state-ref state 1)
	(ptree-hpos it)))

(defun s1.w (state)
  (aand (state-ref state 1)
	(ptree-hword it)))

(declaim (inline s1.r.c s1.r.w))
(defun s1.r.c (state)
  (aand (state-ref state 1)
	(first (ptree-subtrees it))
	(ptree-root-label it)))

(defun s1.r.w (state)
  (aand (state-ref state 1)
	(first (ptree-subtrees it))
	(ptree-hword it)))

(declaim (inline s1.l.c s1.l.w))
(defun s1.l.c (state)
  (aand (state-ref state 1)
	(last1 (ptree-subtrees it))
	(ptree-root-label it)))

(defun s1.l.w (state)
  (aand (state-ref state 1)
	(last1 (ptree-subtrees it))
	(ptree-hword it)))

(declaim (inline s1.h.c))
(defun s1.h.c (state)
  (aand (state-ref state 1)
	(ptree-head-subtree it)
	(ptree-root-label it)))

;; s2
(declaim (inline s2.c s2.t s2.w))
(defun s2.c (state)
  (aand (state-ref state 2)
	(cons (ptree-root-label it) (ptree-complete? it))))

(defun s2.t (state)
  (aand (state-ref state 2)
	(ptree-hpos it)))

(defun s2.w (state)
  (aand (state-ref state 2)
	(ptree-hword it)))

;; s3
(declaim (inline s3.c s3.t s3.w))
(defun s3.c (state)
  (aand (state-ref state 3)
	(cons (ptree-root-label it) (ptree-complete? it))))

(defun s3.t (state)
  (aand (state-ref state 3)
	(ptree-hpos it)))

(defun s3.w (state)
  (aand (state-ref state 3)
	(ptree-hword it)))

(declaim (inline b0.init b0.w b1.init b1.w b2.init b2.w b3.init b3.w))

;; b0
(defun b0.init (state)
  (word-initial (state-nth-word (state-position state) state)))
(defun b0.w (state)
  (word-suffix (state-nth-word (state-position state) state)))

;; b1
(defun b1.init (state)
  (word-initial (state-nth-word (+ (state-position state) 1) state)))
(defun b1.w (state)
  (word-feature (state-nth-word (+ (state-position state) 1)state)))

;; b2
(defun b2.init (state)
  (word-initial (state-nth-word (+ (state-position state) 2) state)))
(defun b2.w (state)
  (word-feature (state-nth-word (+ (state-position state) 2)state)))

;; b3
(defun b3.init (state)
  (word-initial (state-nth-word (+ (state-position state) 3) state)))
(defun b3.w (state)
  (word-feature (state-nth-word (+ (state-position state) 3)state)))

;; nld
(declaim (inline s0.n0.c s0.n1.c))
(defun s0.n0.c (state)
  (aand (nth 0 (state-s0.nld-elements state))
	(ptree-root-label it)))

(defun s0.n1.c (state)
  (aand (nth 1 (state-s0.nld-elements state))
	(ptree-root-label it)))

(declaim (inline s1.n0.c s1.n1.c))
(defun s1.n0.c (state)
  (aand (nth 0 (state-s1.nld-elements state))
	(ptree-root-label it)))

(defun s1.n1.c (state)
  (aand (nth 1 (state-s1.nld-elements state))
	(ptree-root-label it)))

(declaim (inline n0.c n1.c))
(defun n0.c (state)
  (aand (nth 0 (state-rest.nld-elements state))
	(ptree-root-label it)))

(defun n1.c (state)
  (aand (nth 1 (state-rest.nld-elements state))
	(ptree-root-label it)))

;;; Grammatical constraints
(declaim (inline state-left-corner-main-context))
(defun state-left-corner-main-context (state)
  (list (s1.c state)
	(s0.c state)))

(declaim (inline state-attach-main-context))
(defun state-attach-main-context (state)
    (list (s1.c state)
	  (s0.c state)))

(declaim (inline state-e-shift-main-context))
(defun state-e-shift-main-context (state)
  (list (s0.c state)
	(s0.r.c state)))

;;; Local features
(defun state-surface-context-features (state)

  (let ((features nil))

    (push nil features)

    ;; b0
    (push (extract-context-feature state :b0.init) features)
    (push (extract-context-feature state :b0.w) features)

    ;; b1
    (push (extract-context-feature state :b1.init) features)
    (push (extract-context-feature state :b1.w) features)

    ;; b2
    (push (extract-context-feature state :b2.init) features)
    (push (extract-context-feature state :b2.w) features)

    ;; b3
    (push (extract-context-feature state :b3.init) features)
    (push (extract-context-feature state :b3.w) features)

    ;; b0 + b1
    (push (extract-context-feature state :b0.init :b1.init) features)
    (push (extract-context-feature state :b0.w :b1.init) features)
    (push (extract-context-feature state :b0.init :b1.w) features)
    (push (extract-context-feature state :b0.w :b1.w) features)
   
    features))

(defun state-local-context-features (state)
  (let ((features nil))

    ;; s0
    (push (extract-context-feature state :s0.c :s0.t) features)
    (push (extract-context-feature state :s0.c :s0.w) features)

    (push (extract-context-feature state :s0.l.c :s0.l.w) features)
    (push (extract-context-feature state :s0.r.c :s0.r.w) features)
    (push (extract-context-feature state :s0.h.c :s0.w) features)

    ;; s1
    (push (extract-context-feature state :s1.c :s1.t) features)
    (push (extract-context-feature state :s1.c :s1.w) features)

    (push (extract-context-feature state :s1.l.c :s1.l.w) features)
    (push (extract-context-feature state :s1.r.c :s1.r.w) features)
    (push (extract-context-feature state :s1.h.c :s1.w) features)

    ;; s2
    (push (extract-context-feature state :s2.c :s2.t) features)
    (push (extract-context-feature state :s2.c :s2.w) features)

    ;; s3
    (push (extract-context-feature state :s3.c :s3.t) features)
    (push (extract-context-feature state :s3.c :s3.w) features)

    ;; s1 + s0
    (push (extract-context-feature state :s1.w :s0.w) features)
    (push (extract-context-feature state :s1.c :s0.w) features)
    (push (extract-context-feature state :s1.w :s0.c) features)
    (push (extract-context-feature state :s1.c :s0.c) features)

    ;; s0 + b0
    (push (extract-context-feature state :s0.c :b0.init) features)
    (push (extract-context-feature state :s0.c :b0.w) features)
    (push (extract-context-feature state :s0.w :b0.init) features)
    (push (extract-context-feature state :s0.w :b0.w) features)

    ;; s1 + b0
    (push (extract-context-feature state :s1.c :b0.init) features)
    (push (extract-context-feature state :s1.c :b0.w) features)
    (push (extract-context-feature state :s1.w :b0.init) features)
    (push (extract-context-feature state :s1.w :b0.w) features)

    ;; s2 + s1 + s0
    (push (extract-context-feature state :s2.c :s1.c :s0.c) features)
    (push (extract-context-feature state :s2.c :s1.c :s0.w) features)
    (push (extract-context-feature state :s2.c :s1.w :s0.c) features)
    (push (extract-context-feature state :s2.w :s1.c :s0.c) features)

    ;; s1 + s0 + b0
    (push (extract-context-feature state :s1.c :s0.c :b0.init) features)
    (push (extract-context-feature state :s1.w :s0.c :b0.init) features)
    (push (extract-context-feature state :s1.c :s0.w :b0.init) features)
    (push (extract-context-feature state :s1.w :s0.w :b0.init) features)

    features))

(declaim (inline equivalence-key))
(defun equivalence-key (state)
  (list (state-position state)

	(s0.c state)
	(s0.t state)
	(s0.w state)

	(s0.l.c state)
	(s0.l.w state)
	(s0.r.c state)
	(s0.r.w state)
	(s0.h.c state)

	(s1.c state)
	(s1.t state)
	(s1.w state)

	(s1.l.c state)
	(s1.l.w state)
	(s1.r.c state)
	(s1.r.w state)
	(s1.h.c state)

	(s2.c state)
	(s2.t state)
	(s2.w state)

	(s3.c state)
	(s3.t state)
	(s3.w state)
	))

(declaim (inline nld-equivalnece-key))
(defun nld-equivalence-key (state)
  (list (s0.n0.c state)
	(s0.n1.c state)

	(s1.n0.c state)
	(s1.n1.c state)

	(n0.c state)
	(n1.c state)
	))

(defun state-nld-context-features (state)
  (let ((context-features nil))

    (push (extract-context-feature state :s0.n0.c) context-features)
    (push (extract-context-feature state :s0.n1.c) context-features)

    (push (extract-context-feature state :s1.n0.c) context-features)
    (push (extract-context-feature state :s1.n1.c) context-features)

    (push (extract-context-feature state :n0.c) context-features)
    (push (extract-context-feature state :n1.c) context-features)

    context-features))

(defun make-nld-identification-features (action success?)
  (make-features action (list (make-context-feature :nld-identification success?))))
