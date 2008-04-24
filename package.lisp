;;;; lisp-matrix package definition.
;;;; Author: mfh

(defpackage :lisp-matrix
  (:use :cl
        :asdf
        :cffi
        :org.middleangle.foreign-numeric-vector)
  
  (:import-from :fnv
                :ncat))

(in-package :lisp-matrix)
