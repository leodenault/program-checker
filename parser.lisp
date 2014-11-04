;;;; Functions used in parsing the programming language
;;;; used for program checking.

(defun main (argv)
  (let ((tokens
	 (cl-ppcre::split "\\s" (nth 1 argv))))
    (loop for token in tokens
       do (write-line token))
    (sb-ext:exit)))

;;; Parses an integer expression as defined in BNF
;;; in the course textbook.
(defun parse-int-expression ()
  0)
