;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ccQ-toolbox-asdf
  (:use #:CL #:asdf)
  (:nicknames #:ccQTS)
  )

(in-package #:ccQTS)

(defsystem ccQ-toolbox
    :name "ccQ-toolbox"
    :version "0.1.1"
    :maintainer "ccQpein"
    :author "ccQpein"
    :components ((:file "general-tool")
                 (:file "math-tool"
                        :depends-on ("general-tool"))
                 (:file "matrix-tools"
                        :depends-on ("general-tool")))
    )


