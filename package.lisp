;;;; lisp-matrix package definition.
;;;; Author: mfh

(defpackage :lisp-matrix
  (:use :cl
        :cffi
        :org.middleangle.foreign-numeric-vector
	:fiveam)
  
  (:import-from :fnv))

(in-package :lisp-matrix)
