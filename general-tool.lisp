(defpackage #:general-tool
  (:use #:CL)
  (:nicknames #:GT)
  (:export #:with-gensyms
           #:aappend
           #:combine
           #:range
           #:->
           #:->>))

(in-package #:general-tool)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun aappend (l &rest eles)
  (loop with rel = l
     for i in eles
     do (setf rel (append rel (list i)))
     finally (return rel)))

(defmacro combine ((&rest funList) &rest argList)
  "usage: (combine (exp1 exp2 exp3) '(1 2 3))
=> (exp1 (exp2 (exp3 '(1 2 3))))"
  (loop with funL = (copy-list (reverse funList))
        with exp = (cons (car funL) argList)
        for f in (cdr funL)
        do (setf exp (list f exp))
        finally (return exp)))

(defun range (n)
  (loop for i from 0 to (1- n)
     collect i))

(defmacro ->> (x &rest forms)
  "Same usage as Clojure ->>, (->> x form1 form2 ...)
=> (... (form2 (form1 x)))"
  (loop with funL = (copy-list forms)
     with exp = x
     for f in funL
     do (setf exp (append f (list exp)))
     finally (return exp)))

(defmacro -> (x &rest forms)
  "Same usage as Clojure ->, (-> x form1 form2 ...)
=> (... (form2 (form1 x args) args) args)"
  (loop with funL = (copy-list forms)
     with exp = x
     for f in funL
     do (setf exp (append (list (car f) exp) (cdr f)))
     finally (return exp)))
