;;;; Functions used in parsing the programming language
;;;; used for program checking.

(defpackage #:parser
  (:use #:common-lisp #:cl-ppcre #:sb-gray))
(in-package #:parser)

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

(defun parse-int (stream)
  (let ((chars (read-while stream "[\\d-]")))
    (match "-?\\d+" chars)))

(defun parse-var (stream)
  (let ((chars (read-while stream "[\\w\\d-_]")))
    (and
     (match "\\w(?:\\d|\\w|-|_)*" chars)
     (not (match "true|false" chars)))))

(defun parse-math-expression (stream)
  (let ((char (read-char stream nil)))
    (if (or
	 (char-eq char #\+)
	 (char-eq char #\-)
	 (char-eq char #\*))
	(parse-expression stream))))

(defun parse-complex-expression (stream)
  (let ((char (peek-char nil stream nil)))
    (if (char-eq char #\-)
	(progn
	  (read-char stream nil)
	  (parse-expression stream))
	(and
	 (parse-expression stream)
	 (parse-math-expression stream)))))

(defun parse-parenthesized-expression (stream)
  (and
   (char-eq (read-char stream nil) #\()
   (parse-complex-expression stream)
   (char-eq (read-char stream nil) #\))))

(defun parse-expression (stream)
  (let ((char (format nil "~a" (peek-char nil stream nil))))
    (cond
      ((match "[\\d-]" char) (parse-int stream))
      ((match "[\\w_]" char) (parse-var stream))
      ((match "\\(" char) (parse-parenthesized-expression stream)))))

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
	 (char-eq (read-char stream) #\!)
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

(defun main-func (input)
  (let ((stream (make-string-input-stream input)))
    (format nil "'~a' ~:[isn't~;is~] a boolean expression"
	    input (and
		   (parse-boolean stream)
		   (null (peek-char nil stream nil))))))

(defun main (argv)
  (let ((input
	 (nth 1 argv)))
    (write-line (main-func input))
    (sb-ext:exit)))

