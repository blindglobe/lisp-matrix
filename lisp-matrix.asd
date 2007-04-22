;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; TODO: wrap this in a #+(or <supported-systems>) with an error if 
;;; the system isn't supported.
;;;
(asdf:defsystem lisp-matrix
    :name "lisp-matrix"
    :version "0.0.1"
    :author "Mark Hoemmen <mhoemmen@cs.berkeley.edu>"
    :license "BSD sans advertising clause"
    :description "linear algebra library"
    :long-description "Linear algebra library for ANSI Common Lisp; implemented at the lowest level using CFFI to call the BLAS and LAPACK.  Should run on any ANSI CL implementation that supports CFFI."
    :serial t  ;; the dependencies are linear
    :depends-on ("cffi" "org.middleangle.foreign-numeric-vector")
    :components ((:file "package")
		 (:file "macros" :depends-on ("package"))
		 (:file "fnv-matrix" :depends-on ("package" "macros"))
		 (:file "fnv-vector" :depends-on ("package" "macros"))))

