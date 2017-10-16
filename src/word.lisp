(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :lc-parser)

(defstruct word
  occurrence
  initial
  suffix
  pos-tags
  feature
  )

(defun word-string-initial (word-string)
  (cond
    ((scan "^[a-z]" word-string)
     :lower)
    ((scan "^[A-Z]" word-string)
     :upper)
    ((scan "^[0-9]" word-string)
     :digit)
    (t
     (char word-string 0))))

;;; word
(defun word-suffixes (word-string)
  (let* ((word-string (string-downcase (regex-replace-all "[0-9]" word-string "0")))
	 (len (length word-string))
	 (result nil))
    (loop
       :for i :of-type fixnum :from len :downto 1
       :do
       (push (list :suffix (subseq word-string i len)) result))
    (push (list :word word-string) result)
    result))
