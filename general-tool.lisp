;;; general-tool
(defpackage #:general-tool
  (:use #:CL)
  (:nicknames #:GT)
  (:export #:with-gensyms
           #:aappend
           #:combine
           #:range
           #:->
           #:->>
           #:update-hash
           #:pprint-hash-table
           ))

(in-package #:general-tool)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro aappend (l &rest eles)
  `(append ,l ',eles)
  )

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

(defun update-hash (key hash-table fun &rest args)
  "update hash value conveniently"
  (setf (gethash key hash-table)
        (apply fun (gethash key hash-table) args)
        ))

(defun pprint-hash-table (ht)
  (loop for key being the hash-keys of ht
     using (hash-value value)
     do (format t "~S => ~S~%" key value)))

;;; thanks for lispm https://www.reddit.com/r/Common_Lisp/comments/5x4jbs/code_and_macro_from_clojure_to_common_lisp/
(defmacro ->> (x &rest forms)
  "Same usage as Clojure ->>, (->> x form1 form2 ...)
=> (... (form2 (form1 x)))"
  (reduce (lambda (a b)
            (append b (list a)))
          forms
          :initial-value x))

(defmacro -> (x &rest forms &aux (rform x))
  "Same usage as Clojure ->, (-> x form1 form2 ...)
=> (... (form2 (form1 x args) args) args)"
  (dolist (form forms rform)
    (setf rform (append (list (car form))
                        (list rform (cadr form))))))
