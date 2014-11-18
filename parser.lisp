;;;; Functions used in parsing the programming language
;;;; used for program checking.

(defpackage #:parser
  (:use #:common-lisp #:cl-ppcre #:sb-gray))
(in-package #:parser)

;;; ******************************************
;;; Constants
;;; ******************************************

(defconstant var-regex "[a-zA-Z](?:\\d|\\w)*")
(defconstant reserved-keywords "true|false|if|else|while|T|F")
(defconstant usage-message
  "usage: program-checker <precondition> <program> <postcondition> [<variants>]")
(defconstant error-message
  "The program is not valid. Please verify the syntax and try again.")
(defconstant inv-var-error "The numbers of invariants and variants don't match. Please ensure to input equal numbers of each.")
(defconstant math-operators '(#\+ #\- #\*))

;;; ******************************************
;;; Global Variables
;;; ******************************************

(defparameter *invariants* '())
(defparameter *variants* '())
(defparameter *partial-proofs* '())
(defparameter *total-proofs* '())

;;; ******************************************
;;; Data structures
;;; ******************************************

(defstruct command var expression)
(defstruct if-command boolean then-command-list else-command-list)
(defstruct while-command boolean command-list)

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

(defun match (regex input)
  (multiple-value-bind (start end)
      (scan regex input)
    (if (or (null start) (null end))
	nil
	(and
	 (= 0 start)
	 (= (length input) end)))))

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
	chars)))

;;; Parses a variable identifier from stream
(defun parse-var (stream)
  (let ((chars (read-while stream "[\\w\\d]")))
    (if (and
	 (match var-regex  chars)
	 (not (match reserved-keywords  chars)))
	chars)))

;;; Parses a left curly brace from the stream
(defun parse-left-brace (stream)
  (match "\\s*{\\s*" (read-while stream "[{\\s]")))

;;; Parses a right curly brace from the stream
(defun parse-right-brace (stream)
  (and (read-while stream "\\s")
       (char-eq (read-char stream nil) #\})
       (read-while stream "\\s")))

;;; ******************************************
;;; Expression parsing functions
;;; ******************************************

;;; Parses the operator and the second operand
;;; of a mathematical expression from stream
(defun parse-math-expression (stream)
  (let ((char (read-char stream nil))
	(result ""))
    (if (and (member char math-operators)
	     (setf result (parse-expression stream)))
	(format nil "~a~a" char result))))

(defun parse-complex-expression (stream)
  ;; Save the current position of the stream
  (let ((stream-pos (stream-file-position stream))
	(expr-result "")
	(math-result ""))
    (if (and
	 (setf expr-result (parse-expression stream))
	 (setf math-result (parse-math-expression stream)))
	
	(format nil "~a~a" expr-result math-result)

	(progn
	  ;; Restore the stream to its original position
	  (stream-file-position stream stream-pos)
	  (if (and
	       (char-eq (read-char stream nil) #\-)
	       (setf expr-result (parse-expression stream)))
	      (format nil "-~a" expr-result))))))

(defun parse-parenthesized-expression (stream)
  (let ((result ""))
    (if (and
	 (char-eq (read-char stream nil) #\()
	 (setf result (parse-complex-expression stream))
	 (char-eq (read-char stream nil) #\)))
	(format nil "(~a)" result))))

(defun parse-expression (stream)
  (let ((char (format nil "~a" (peek-char nil stream nil))))
    (cond
      ((match "[\\d-]" char) (parse-int stream))
      ((match "\\w" char) (parse-var stream))
      ((match "\\(" char) (parse-parenthesized-expression stream)))))

;;; ******************************************
;;; Boolean parsing functions
;;; ******************************************

(defun parse-compound-boolean (stream)
  (let ((char (read-char stream nil)))
    (cond ((char-eq char #\&)
	   (format nil "/~a~a" #\\ (parse-boolean stream)))
	  ((and (char-eq char #\|)
	       (char-eq (read-char stream nil) #\|))
	  (format nil "~a/~a" #\\ (parse-boolean stream))))))

(defun parse-complex-boolean (stream)
  (let ((char (peek-char nil stream nil)))
    (if (char-eq char #\!)
	(progn
	  (read-char stream nil)
	  (format nil "~a~a" #\~ (parse-boolean stream)))
	;; Save the position of the stream for look-ahead
	(let ((stream-pos (stream-file-position stream))
	      (op-char nil)
	      (result ""))
	  (if (setf result (parse-expression stream))
	      (progn
		(setf op-char (read-char stream nil))
		(if
		  (or (char-eq op-char #\<) (char-eq op-char #\>))
		  (format nil "~a~a~a"
			  result op-char (parse-expression stream))
		  (let ((next-char (read-char stream nil)))
		    (cond
		      ((and (char-eq op-char #\!) (char-eq next-char #\=))
		       (format nil "~a(~a=~a)" #\~
			       result (parse-expression stream)))
		      ((and (char-eq op-char #\=) (char-eq next-char #\=))
		       (format nil "~a=~a"
			       result (parse-expression stream)))))))
	      (progn
		;; Restore the stream at its original position
		(stream-file-position stream stream-pos)
		(if (setf result (parse-boolean stream))
		    (format nil "~a~a"
			    result (parse-compound-boolean stream)))))))))

(defun parse-boolean (stream)
  (if (char-eq (peek-char nil stream nil) #\()
      (let ((result ""))
	(if (and
	     (char-eq (read-char stream nil) #\()
	     (setf result (parse-complex-boolean stream))
	     (char-eq (read-char stream nil) #\)))
	    (format nil "(~a)" result)))
      (let ((value (read-while stream "[truefals]")))
	(cond ((match "true" value) "T")
	      ((match "false" value) "F")))))

;;; ******************************************
;;; Command parsing functions
;;; ******************************************

(defun parse-assignment (stream)
  (let ((comm (make-command)))
    (if
     (and
      (setf (command-var comm) (parse-var stream))
      (char-eq (read-char stream nil) #\=)
      (setf (command-expression comm) (parse-expression stream)))
     comm)))

(defun parse-if (stream)
  (let ((chars (progn
		 (format nil "~a~a"
			 (read-char stream nil)
			 (read-char stream nil))))
	(comm (make-if-command)))
    (if (and (match "if" chars)
	     (read-while stream "\\s")
	     (setf (if-command-boolean comm) (parse-boolean stream))
	     (parse-left-brace stream)
	     (setf (if-command-then-command-list comm)
		   (parse-command-list stream))
	     (parse-right-brace stream)
	     (match "else" (read-while stream "[else]"))
	     (parse-left-brace stream)
	     (setf (if-command-else-command-list comm)
		   (parse-command-list stream))
	     (parse-right-brace stream))
	comm)))

(defun parse-while (stream)
  (let ((chars (read-while stream "[while]"))
	(comm (make-while-command)))
    (if (and (match "while" chars)
	     (read-while stream "\\s")
	     (setf (while-command-boolean comm) (parse-boolean stream))
	     (parse-left-brace stream)
	     (setf (while-command-command-list comm)
		   (parse-command-list stream))
	     (parse-right-brace stream))
	comm)))

(defun parse-command (stream)
  ;; Read the first few characters and then reset the stream
  ;; to know how to parse the commands
  (let ((stream-pos (stream-file-position stream))
	(chars (read-while stream "[\\w\\d]")))
    (stream-file-position stream stream-pos)
    (cond
      ((match "if" chars) (parse-if stream))
      ((match "while" chars) (parse-while stream))
      (t (parse-assignment stream)))))

(defun parse-command-list (stream)
  (loop
     for comm = (parse-command stream)
     for char = (progn (read-while stream "\\s")
		       (peek-char nil stream nil))
     for is-semi-colon = (char-eq char #\;)
     for is-command = (command-p comm)
     for continue = t

     while (not (null comm))
     collect comm
     
     do (if is-command
	    (if is-semi-colon
		(read-char stream nil)
		(setf continue nil)))

     do (read-while stream "\\s")
     while continue))

;;; ******************************************
;;; Command bubbling functions
;;; ******************************************

(defun bubble-assignment (command prop-cond)
  (regex-replace-all 
   (command-var command)
   prop-cond
   (command-expression command)))

(defun bubble-if (command prop-cond)
  (let ((a1 (bubble-commands
	     (if-command-then-command-list command)
	     prop-cond))
	(a2 (bubble-commands
	     (if-command-else-command-list command)
	     prop-cond))
	(bool (if-command-boolean command)))
    (format nil "(~a->~a)/~a(~a~a->~a)" bool a1 #\\ #\~ bool a2)))

(defun bubble-partial-while (command prop-cond)
  (let ((invariant (pop *invariants*)))  
    (if (null invariant)
	(setf invariant "inv"))  
    (let ((implied (bubble-commands
		    (while-command-command-list command)
		    invariant))
	  (bool (while-command-boolean command)))
      (push (format nil "(~a/~a~a~a)->(~a)"
		    invariant #\\ #\~ bool prop-cond) *partial-proofs*)
      (push (format nil "(~a/~a~a)->(~a)"
		    invariant #\\ bool implied) *partial-proofs*)
      invariant)))

(defun bubble-total-while (command prop-cond)
  (let ((invariant (pop *invariants*))
	(variant (pop *variants*)))
    (if (null invariant)
	(setf invariant "inv"))
    (if (null variant)
	(setf variant "var"))
    (let ((implied (bubble-commands
		    (while-command-command-list command)
		    (format nil "~a/~a0<=~a<init" invariant #\\ variant)))
	  (bool (while-command-boolean command)))
      (push (format nil "(~a/~a~a~a)->(~a)"
		    invariant #\\ #\~ bool prop-cond) *total-proofs*)
      (push (format nil "(~a/~a~a/~a0<=~a=init)->(~a)"
		    invariant #\\ bool #\\ variant implied) *total-proofs*)
      (format nil "~a/~a0<=~a" invariant #\\ variant))))

(defun bubble-commands (commands postcondition
			&optional (total-correctness nil))
  (if (null commands)
      nil
      (let ((reverse-commands (reverse commands))
	    (current-prop-condition postcondition))
	(loop for command in reverse-commands
	   if (command-p command)
	   do (setf current-prop-condition
		    (bubble-assignment command current-prop-condition))
	   if (if-command-p command)
	   do (setf current-prop-condition
		    (bubble-if command current-prop-condition))
	   if (while-command-p command)
	   do (progn
		(setf current-prop-condition
		      (if (null total-correctness)
			  (bubble-partial-while
			   command current-prop-condition)
			  (bubble-total-while
			   command current-prop-condition)))))
	current-prop-condition)))

;;; ******************************************
;;; Main functions
;;; ******************************************

(defun build-parse-tree (input)
  (let* ((stream (make-string-input-stream input))
	 (comm-list (parse-command-list stream)))
    (if (and (not (null comm-list))
	     (null (read-char stream nil)))
	comm-list)))

(defun check-program (precondition program postcondition invariants variants)
  (setf *partial-proofs* '())
  (setf *total-proofs* '())
  (setf *invariants* (reverse invariants))
  (setf *variants* (reverse variants))
  (let* ((parse-tree (build-parse-tree program))
	 (invariants-copy (copy-list *invariants*))
	 (partial-correctness 
	  (bubble-commands parse-tree
	   (format nil "(~a)" postcondition))))
    (setf *invariants* (copy-list invariants-copy))
    (let ((total-correctness 
	  (bubble-commands parse-tree
	    (format nil "(~a)" postcondition) t)))
      (if (or (null partial-correctness) (null total-correctness))
	  nil
	  (format nil "Partial correctness proofs:~%(~a) -> ~a~{~%~a~}~%~%Total correctness proofs:~%(~a) -> ~a~{~%~a~}"
		  precondition partial-correctness *partial-proofs*
		  precondition total-correctness *total-proofs*)))))

(defun main (argv)
  (if (not (>= (length argv) 4))
      (progn (write-line usage-message)
	     (sb-ext:exit))
      (let ((precondition (nth 1 argv))
	     (program (nth 2 argv))
	     (postcondition (nth 3 argv))
	     (inv-var-length (- (length argv) 4)))
	(if (not (= (mod inv-var-length 2) 0))
	    (write-line inv-var-error)
	    (let* ((inv-length (/ inv-var-length 2))
		  (invariants (subseq argv 4 (+ inv-length 4)))
		  (variants (subseq argv (+ inv-length 4) (length argv)))
		  (result (check-program
			   precondition
			   program
			   postcondition
			   invariants
			   variants)))
	      (if (null result)
		  (write-line error-message)
		  (write-line result))
	      (sb-ext:exit))))))
