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
    :depends-on ("cffi"
		 "org.middleangle.foreign-numeric-vector"
		 "org.middleangle.cl-blapack"
                 "fiveam")
    :components
    ((:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "macros" :depends-on ("package"))
     (:file "matrix" :depends-on ("package"))
     (:file "matrix-lisp-array" :depends-on ("matrix" "utils"))
     (:file "matrix-foreign-array" :depends-on ("matrix" "utils"))
     (:file "fnv-vector" :depends-on ("package" "macros" "utils"))
     (:file "lapack-utils" :depends-on ("matrix-foreign-array"
                                        "fnv-vector"))
     (:file "lapack-methods" :depends-on ("lapack-utils"))
     (:file "tests" :depends-on ("matrix" "matrix-lisp-array"
                                          "matrix-foreign-array"
                                          "fnv-vector"
                                          "lapack-utils"
                                          "lapack-methods")))
    :in-order-to ((test-op (load-op lisp-matrix)))
    :perform (test-op :after (op c)
                      (funcall (intern "RUN!" 'fiveam)
                               (intern "TESTS" 'lisp-matrix))))

;; keep ASDF thinking that the test operation hasn't been done
(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'lisp-matrix))))
  (values nil))
