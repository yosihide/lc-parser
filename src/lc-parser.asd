(in-package :cl-user)

(defpackage :lc-parser.system
  (:use :cl
	:asdf))

(in-package :lc-parser.system)

(defsystem :lc-parser
    :author "Yoshihide Kato"
    :depends-on (:cl-ppcre)
    :components ((:file "packages")
		 ;; utilities
		 (:file "macro")
		 (:file "hash-table-lib")
		 (:file "counter")
		 (:file "list-lib")
		 (:file "memoize")
		 (:file "stream-lib")
		 (:file "string-lib")
		 (:file "utils")

		 (:file "ptb")
		 (:file "node")
		 (:file "label")
		 (:file "corpus")
		 (:file "action")
		 (:file "feature")
		 (:file "perceptron")
		 (:file "word")
		 (:file "ptree")
		 (:file "state")
		 (:file "extract-feature")
		 (:file "grammar")
		 (:file "parser")))
