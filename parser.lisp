;;;; Functions used in parsing the programming language
;;;; used for program checking.

;;; Reads whitespace up until the first non-whitespace
;;; character. Returns nil upon EOF.
(defun read-whitespace (in)
  (loop for char = (read-char in nil)
       when (not (member char '(#\Space \#Newline #\Tab)))
	   return char))

(defun main (argv)
  (let ((input (nth 1 argv)))
    (write-line
     (string
      (read-whitespace (make-string-input-stream input))))
    (sb-ext:exit)))

;;; Parses an integer expression as defined in BNF
;;; in the course textbook.
(defun parse-int-expression ()
  0)
