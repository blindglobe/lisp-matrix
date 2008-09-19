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
    :long-description "Linear algebra library for ANSI Common Lisp;
    implemented at the lowest level using CFFI to call the BLAS and
    LAPACK.  Should run on any ANSI CL implementation that supports
    CFFI.  Also considers lisp-centric and foreign-centric storage, in
    a manner friendly to the other storage facility."
    :serial t
    :depends-on (:cffi
                 :fiveam
		 :lift ;; yes, Tony (me) is a PITA...
		 :org.middleangle.foreign-numeric-vector
		 :org.middleangle.cl-blapack
		 :ffa
                 :cl-utilities)
    :components
    ((:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "macros" :depends-on ("package"))
     (:file "matrix" :depends-on ("package"))
     (:file "vector" :depends-on ("matrix"))
     (:file "matrix-implementations" :depends-on ("matrix" "vector"))
     (:file "matrix-lisp-array" :depends-on ("matrix"
                                             "matrix-implementations"
                                             "utils"))
     (:file "matrix-foreign-array" :depends-on ("matrix"
                                                "matrix-implementations"
                                                "utils"))
     (:file "lapack-utils" :depends-on ("matrix-foreign-array"
                                        "matrix-lisp-array"))
     (:file "lapack-methods" :depends-on ("lapack-utils"))
     (:file "matrix-operations" :depends-on ("lapack-methods"))
     (:file "tests" :depends-on ("matrix" "matrix-lisp-array"
                                          "matrix-foreign-array"
                                          "matrix-operations"
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
