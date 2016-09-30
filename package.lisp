(in-package #:CL-USER)

(defpackage #:general-tool
  (:use #:CL)
  (:nicknames #:GT)
  (:export #:with-gensyms
           #:aappend))

(defpackage #:math-tool
  (:use #:CL #:GT)
  (:nicknames #:MT)
  (:export #:standard-deviation
           #:gen-random-num
           #:sigma
           ))


(defpackage #:matrix-tool
  (:use #:CL #:GT)
  (:nicknames #:MXT)
  (:export #:*list-to-array
           #:*array-to-list
           #:array-slice
           #:array-slice-col
           #:array-multiply
           #:matrix-norm-2
           #:point-distance
           #:points-average
           ))

;;;;;;;;;;;; Enter tool-box.lisp ;;;;;;;;;;;
(defpackage #:tool-box
  (:use #:CL
        #:GT
        #:MT
        #:MXT)
  (:nicknames #:TB))
