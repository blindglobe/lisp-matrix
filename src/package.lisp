;;;; lisp-matrix package definition.
;;;; Author: mfh

(in-package :cl-user)

(defpackage :lisp-matrix
  (:use :cl
        :cffi
        :cl-utilities
        :org.middleangle.foreign-numeric-vector
        :org.middleangle.cl-blapack
	:ffa
	:fiveam)
  (:import-from :fnv)
  (:export make-matrix make-matrix*  ;; basic instantiations
	   strides-class
	   strides unit-strides-p
	   window-class window
	   transpose-class transpose transposed-p
	   
	   zero-offset-p

	   ones zeros eye rand ;; types 
	   copy copy! copy*
	   copy-maybe copy-maybe*
	   fill-matrix

	   m= m* m+ m- 
	   v= v* v+ v- 

	   print-object
	   mref data row col
	   nelts  nrows ncols
	   matrix-dimension matrix-dimensions
	   orientation valid-orientation-p opposite-orientation
	   flatten-matrix-indices flatten-matrix-indices-1

	   la-simple-matrix-double  la-simple-matrix-integer
	   la-simple-matrix-single
	   ;; Next 2 symbols are guesses at... wrong?
	   la-simple-matrix-complex 
	   la-simple-matrix-fixnum 
	   
	   la-simple-vector-double
	   la-simple-vector-integer

	   ;; Next paragrah of symbols are guesses... wrong?
	   fa-simple-matrix-double  fa-simple-matrix-integer
	   fa-simple-matrix-complex  fa-simple-matrix-float 
	   fa-simple-vector-double fa-simple-vector-integer
	   fa-simple-matrix-fixnum 

	   *implementations*

	   col-vector-p
	   make-vector
	   parent
	   real-stride
	   row-vector-p

	   ))


(defpackage :lisp-matrix-user
  (:documentation "Experimentation package for lisp-matrix")
  (:use :cl
	:lisp-matrix))


(defpackage :lisp-matrix-unittests
  (:documentation "Unit, validation, and regression testing for lisp-matrix")
  (:use :common-lisp
	:lift
	:lisp-matrix)
  (:export run-lisp-matrix-tests))