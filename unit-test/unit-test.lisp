;;;; Unit Test Framework
;;;; From Practical Common Lisp, Chapter 9
;;;; Matthew Nunes

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest variables) &body body)
  `(let
       ,(loop for variable in variables collecting `(,variable (gensym)))
     ,@body))

(defmacro combine-results (&rest results)
  (with-gensyms (result-p)
    `(progn 
       (let ((,result-p t))
	 ,@(loop for result in results collecting `(unless ,result (setf ,result-p nil)))
	 ,result-p))))

(defmacro check (&rest forms)
  `(combine-results
       ,@(loop for form in forms collecting `(report-result ,form ',form))))
