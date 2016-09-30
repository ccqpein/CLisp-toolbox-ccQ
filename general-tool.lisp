(load "./package.lisp")
(in-package #:general-tool)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro aappend (l &rest eles)
  "l must be symbol not expression. For example, (aappend a 2 3 4) is fine, (aappend '(1 2) 2 3) and (aappend (list 2 3) 2 2) will issue error"
  (with-gensyms (elel)
    `(let ((,elel (list ,@eles)))
       (loop for i in ,elel do
            (setf ,l (append ,l (list i))))
       ,l)))
