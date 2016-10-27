(defpackage #:general-tool
  (:use #:CL)
  (:nicknames #:GT)
  (:export #:with-gensyms
           #:aappend
           #:combine))

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

(defmacro combine ((&rest funList) &rest argList)
  `(,@(loop with funL = (copy-list (reverse funList))
         with exp = (cons (car funL) argList)
         for f in (cdr funL)
         do (setf exp (list f exp))
         finally (return exp))))
