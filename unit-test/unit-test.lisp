;;;; Unit Test Framework
;;;; From Practical Common Lisp, Chapter 9
;;;; Matthew Nunes

(defvar *unit-test-name* nil)

(defmacro with-gensyms ((&rest variables) &body body)
  "Macro which (gensym)s a list a variables for use within 
  a macro expansion's body."
  `(let
       ,(loop for variable in variables collecting `(,variable (gensym)))
     ,@body))

(defmacro combine-results (&rest results)
  "Works like (AND ...) but will evaluate every single 
  form in results, rather than short-circuiting."
  (with-gensyms (result-p)
    `(progn 
       (let ((,result-p t))
	 ,@(loop for result in results collecting `(unless ,result (setf ,result-p nil)))
	 ,result-p))))

(defmacro check (&rest forms)
  "Reports the (combine-results ...) of running 
  (report-result ...) on each form in forms."
  `(combine-results
       ,@(loop for form in forms collecting `(report-result ,form ',form))))

(defmacro deftest (name parameters &body body)
  "Defines a unit test, from within which you can call 
  other tests defined with (deftest ...) or call (check ...)"
  `(defun ,name ,parameters
     (let ((*unit-test-name* (append *unit-test-name* (list ',name))))
       ,@body)))

(defun report-result (result form)
  "Prints FAIL or pass, the originating test function hierarcy,
  and the tested form, and returns the evaluated result, T or NIL."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *unit-test-name* form)
  result)
