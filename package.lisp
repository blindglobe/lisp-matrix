;;;; lisp-matrix package definition.
;;;; Author: mfh

(in-package :cl-user)

(defpackage :lisp-matrix
  (:use :cl
        :cffi
        :cl-utilities
        :org.middleangle.foreign-numeric-vector
        :cl-blapack
	:ffa
	:fiveam)
  (:import-from :fnv)
  (:export make-matrix make-matrix*  ;; basic instantiations
	   strides-class window-class transpose-class
	   strides window transpose 
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
	   la-simple-matrix-complex  la-simple-matrix-float ;; guessed! wrong?
	   
	   la-simple-vector-double la-simple-vector-integer
	   ))

(defpackage :lisp-matrix-user
  (:use :cl
	:lisp-matrix))
