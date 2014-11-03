(ql:quickload :parenscript)

(defun compile-to-file (from to)
	(with-open-file (str to
		                 :direction :output
		                 :if-exists :supersede
		                 :if-does-not-exist :create)
	  (format str (parenscript:ps-compile-file from))))

(defun walk-directory (directory pattern)
	(directory (merge-pathnames pattern directory)))

;(defun compile (compiled-dir dest-dir)
;	)
