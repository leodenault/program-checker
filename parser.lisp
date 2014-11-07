;;;; Functions used in parsing the programming language
;;;; used for program checking.

(defpackage #:parser
  (:use #:common-lisp #:cl-ppcre #:sb-gray))
(in-package #:parser)

;;; ******************************************
;;; Constants
;;; ******************************************

(defconstant var-regex "[a-zA-Z](?:\\d|\\w)*")
(defconstant reserved-keywords "true|false|if|else|while")
(defconstant usage-message
  "usage: program-checker <precondition> <program> <postcondition>")
(defconstant error-message
  "The program is not valid. Please verify the syntax and try again.")

;;; ******************************************
;;; Data structures
;;; ******************************************

(defstruct command var expression)

;;; ******************************************
;;; Helper functions
;;; ******************************************

;;; Helper function for checking if two chars
;;; are equal with the possibility of char1 or
;;; char2 being nil
(defun char-eq (char1 char2)
  (and  
   (not
    (or (null char1) (null char2)))
   (char= char1 char2)))

(defun extract-strings (input starts ends)
  (loop for i from 0 to (1- (length starts))
   for start = (aref starts i)
   for end = (aref ends i)
     collect (subseq input start end)))

(defun match (regex input)
  (multiple-value-bind (start end starts ends)
      (scan regex input)
    (if (or (null start) (null end))
	nil
	(values
	 (and
	  (= 0 start)
	  (= (length input) end))
	 (extract-strings input starts ends)))))

(defun read-while (stream regex)
  (format nil "~{~a~}"
	  (loop
	     for char = (peek-char nil stream nil)
	     while (match regex (format nil "~a" char))
	     while (not (null char))
	     do (read-char stream nil)
	     collect char)))

;;; Parses an integer from stream
(defun parse-int (stream)
  (let ((chars (read-while stream "[\\d-]")))
    (if (match "-?\\d+" chars)
	(values t chars))))

;;; Parses a variable identifier from stream
(defun parse-var (stream)
  (let ((chars (read-while stream "[\\w\\d]")))
    (if (and
	 (match var-regex  chars)
	 (not (match reserved-keywords  chars)))
	(values t chars))))

;;; ******************************************
;;; Expression parsing functions
;;; ******************************************

;;; Parses the operator and the second operand
;;; of a mathematical expression from stream
(defun parse-math-expression (stream)
  (let ((char (read-char stream nil))
	(result ""))
    (if (and (or
	      (char-eq char #\+)
	      (char-eq char #\-)
	      (char-eq char #\*))
	     (multiple-value-bind (parse-result value)
		 (parse-expression stream)
	       (setf result value)
	       parse-result))
	(values t (format nil "~a~a" char result)))))

(defun parse-complex-expression (stream)
  ;; Save the current position of the stream
  (let ((stream-pos (stream-file-position stream))
	(expr-result "")
	(math-result ""))
    (if (and
	 (multiple-value-bind (parse-result value)	 
	     (parse-expression stream)
	   (setf expr-result value)
	   parse-result)
	 (multiple-value-bind (parse-result value)
	     (parse-math-expression stream)
	   (setf math-result value)
	   parse-result))

	(values t (format nil "~a~a" expr-result math-result))

	(progn
	  ;; Restore the stream to its original position
	  (stream-file-position stream stream-pos)
	  (if (and
	       (char-eq (read-char stream nil) #\-)
	       (multiple-value-bind (parse-result value)
		   (parse-expression stream)
		 (setf expr-result value)
		 parse-result))
	      (values t (format nil "-~a" expr-result)))))))

(defun parse-parenthesized-expression (stream)
  (let ((result ""))
    (if (and
	 (char-eq (read-char stream nil) #\()
	 (multiple-value-bind (parse-result value)
	     (parse-complex-expression stream)
	   (setf result value)
	   parse-result)
	 (char-eq (read-char stream nil) #\)))
       (values t (format nil "(~a)" result)))))

(defun parse-expression (stream)
  (let ((char (format nil "~a" (peek-char nil stream nil))))
    (multiple-value-bind (parse-result value)
	(cond
	  ((match "[\\d-]" char) (parse-int stream))
	  ((match "\\w" char) (parse-var stream))
	  ((match "\\(" char) (parse-parenthesized-expression stream)))
      (values parse-result value))))

;;; ******************************************
;;; Boolean parsing functions
;;; ******************************************

(defun parse-compound-boolean (stream)
  (let ((char (read-char stream nil)))
    (and
     (or (char-eq char #\&)
	 (and (char-eq char #\|)
	      (char-eq (read-char stream nil) #\|)))
     (parse-boolean stream))))

(defun parse-complex-boolean (stream)
  (let ((char (peek-char nil stream nil)))
    (if (char-eq char #\!)
	(and
	 (char-eq (read-char stream nil) #\!)
	 (parse-boolean stream))
	;; Save the position of the stream for look-ahead
	(let ((stream-pos (stream-file-position stream)))
	  (if (parse-expression stream)
	      (and
	       (char-eq (read-char stream nil) #\<)
	       (parse-expression stream))
	      (progn
		;; Restore the stream at its original position
		(stream-file-position stream stream-pos)
		(and (parse-boolean stream)
		     (parse-compound-boolean stream))))))))

(defun parse-boolean (stream)
  (if (char-eq (peek-char nil stream nil) #\()
      (and
       (char-eq (read-char stream nil) #\()
       (parse-complex-boolean stream)
       (char-eq (read-char stream nil) #\)))
      (let ((value (read-while stream "[truefals]")))
	(or (match "true" value)
	    (match "false" value)))))

;;; ******************************************
;;; Command parsing functions
;;; ******************************************

(defun parse-simple-command (stream)
  (let ((comm (make-command)))
    (if
     (and
      (multiple-value-bind (var-parsed var)
	  (parse-var stream)
	(setf (command-var comm) var)
	var-parsed)
      (char-eq (read-char stream nil) #\=)
      (multiple-value-bind (expr-parsed expr)
	  (parse-expression stream)
	(setf (command-expression comm) expr)
	expr-parsed))
     comm)))

(defun parse-command (stream)
  (loop
     for comm = (parse-simple-command stream)
     for char = (progn (read-while stream "\\s")
		       (read-char stream nil))
     for next-char = (progn (read-while stream "\\s")
			    (peek-char nil stream nil))
     for is-semi-colon = (char-eq char #\;)
     for null-char = (null char)
       
     collect comm
     if (or (null comm)
	    (not (or is-semi-colon null-char)))
     return nil
       
     while (not (or
		 null-char
		 (and is-semi-colon
		      (null next-char))))))

;;; ******************************************
;;; Command bubbling functions
;;; ******************************************

(defun bubble-assignment (command prop-cond)
  (regex-replace-all 
   (command-var command)
   prop-cond
   (command-expression command)))

(defun bubble-commands (commands postcondition)
  (if (null commands)
      nil
      (let ((reverse-commands (reverse commands))
	    (current-prop-condition postcondition))
	(loop for command in reverse-commands
	   if (command-p command)
	   do (setf current-prop-condition
		    (bubble-assignment command current-prop-condition)))
	current-prop-condition)))

;;; ******************************************
;;; Main functions
;;; ******************************************

(defun build-parse-tree (input)
  (let ((stream (make-string-input-stream input)))
	(parse-command stream)))

(defun check-program (precondition program postcondition)
  (let ((result 
	 (bubble-commands (build-parse-tree program) postcondition)))
    (if (null result)
	nil
	(format nil "~a -> ~a" precondition result))))

(defun main (argv)
  (if (not (= (length argv) 4))
      (progn (write-line usage-message)
	     (sb-ext:exit))
      (let* ((precondition (nth 1 argv))
	     (program (nth 2 argv))
	     (postcondition (nth 3 argv))
	     (result (check-program precondition program postcondition)))
	(if (null result)
	    (write-line error-message)
	    (write-line result))
	(sb-ext:exit))))
