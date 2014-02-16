;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; TODO: wrap this in a #+(or <supported-systems>) with an error if 
;;; the system isn't supported.
;;;
(in-package :cl-user)

(defpackage :lisp-matrix-asd
  (:use :cl :asdf))

(in-package :lisp-matrix-asd)

(defsystem lisp-matrix
    :name "lisp-matrix"
    :version "0.3"
    :author "Mark Hoemmen <mhoemmen@cs.berkeley.edu>"
    :license "BSD sans advertising clause" ;; sort of MIT
    :description "linear algebra library"
    :long-description "Linear algebra library for ANSI Common Lisp;
    implemented at the lowest level using CFFI to call the BLAS and
    LAPACK.  Should run on any ANSI CL implementation that supports
    CFFI.  Uses both lisp-centric and foreign-centric storage, in a
    manner friendly to the complimentary other storage facility."
    :serial t
    :depends-on (:cffi
		 :cl-utilities
		 :xarray ; we will use this for general indexing
                 ;; :fiveam  ;;see below for its replacement...
		 :lift ;; yes, Tony (me) is a PITA...
		 :org.middleangle.foreign-numeric-vector
		 :org.middleangle.cl-blapack
		 :ffa)
    :components
    ((:module
      "package-init"
      :pathname #p "src/"
      :components
      ((:file "package")))

     (:module
      "basics"
      :pathname #p"src/"
      :depends-on ("package-init") 
      :components
      ((:file "utils"  )
       (:file "macros" )
       (:file "matrix" )
       (:file "vector" :depends-on ("matrix"))
       (:file "data-transform" :depends-on ("matrix" "vector"))
       (:file "matrix-implementations" :depends-on ("matrix" "vector"))))

     (:module
      "implementations"
      :pathname #p"src/"
      :depends-on ("package-init" "basics")
      :serial t
      :components
      ((:file "matrix-lisp-array")
       (:file "matrix-foreign-array")
       ;; probably should move the remainder into a numerical linear
       ;; algebra place.
       (:file "lapack-utils" :depends-on ("matrix-foreign-array"
					  "matrix-lisp-array"))
       (:file "lapack-methods" :depends-on ("lapack-utils"))
       (:file "lapack-cholesky" :depends-on ("lapack-utils"))
       (:file "lapack-lu" :depends-on ("lapack-utils"))
       (:file "lapack-qr" :depends-on ("lapack-utils"))
       (:file "lapack-svd" :depends-on ("lapack-utils"))

       (:file "lapack-ls" :depends-on ("lapack-utils"))

       (:file "matrix-operations" :depends-on ("lapack-methods"
					       "lapack-cholesky"
					       "lapack-lu"
					       "lapack-qr"))))
     (:module
      "api"
      :pathname #p"src/"
      :depends-on ("basics" "implementations") 
      :components
      ((:file "numerical-linear-algebra")))

     (:module
      "testing"
      :pathname #p "src/unittests/"
      :depends-on ("implementations")
      :components
      ((:file "unittests")
       (:file "unittests-transform" :depends-on ("unittests"))
       (:file "unittests-matrix" :depends-on ("unittests"))
       (:file "unittests-matrix-view" :depends-on ("unittests" "unittests-matrix"))
       (:file "unittests-matrix-lapack" :depends-on ("unittests" "unittests-matrix"))
       (:file "unittests-vector" :depends-on ("unittests")))))

#|
     :in-order-to ((test-op (load-op lisp-matrix)))
     :perform (test-op :after (op c)
                       (funcall (intern "RUN!" 'fiveam)
                                (intern "TESTS" 'lisp-matrix)))
|#
    )
