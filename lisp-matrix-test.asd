;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(asdf:defsystem :lisp-matrix-test
    :name "lisp-matrix-test"
    :version "1"
    :author "AJ Rossini <blindglobe@gmail.com>"
    :license "BSD sans advertising clause"
    :description "linear algebra library test suite"
    :long-description "Linear algebra library test suite to help
    development for lisp-matrix."
    :serial t
    :depends-on (:lift
                 :lisp-matrix)
    :components
    ((:module
      "unittest"
      :pathname #p"src/unittests/"
      :components ((:file "unittests")))
     (:file "examples" :depends-on ("unittest")))

;;    :in-order-to ((test-op (load-op lisp-matrix)))
    ;; the following is for fiveam, need to modify for lift
;;    :perform (test-op :after (op c)
;;    (funcall (intern "RUN!" 'fiveam)
;;    (intern "TESTS" 'lisp-matrix)))
)

;; keep ASDF thinking that the test operation hasn't been done
#|
(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'lisp-matrix))))
  (values nil))
|#