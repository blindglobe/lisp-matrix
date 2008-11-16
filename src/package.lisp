;;;; lisp-matrix package definition.
;;;; Author: mfh

(in-package :cl-user)

(defpackage :lisp-matrix
  (:use :cl
        :cffi
	;; :fiveam
        :cl-utilities
        :org.middleangle.foreign-numeric-vector
        :org.middleangle.cl-blapack
	:ffa)
  (:import-from :fnv) ;; do we really need this?  We are using it!
  (:export make-matrix make-matrix*  ;; basic instantiations
	   strides-class
	   strides unit-strides-p
	   window-class window
	   transpose-class transpose transposed-p
	   
	   zero-offset-p
	   offset
	   row-offset col-offset
	   row-stride col-stride

	   ones zeros eye rand ;; types 
	   copy copy! copy*
	   copy-maybe copy-maybe*
	   fill-matrix

	   m= m* m+ m-
	   m.* m.+ m.- 
	   v= v* v+ v-
	   ;; v* v+ v-  ; these are inherited from m-based ops, but
	   ;; have a slight issue with still needing a v.* variant,
	   ;; since v* would inherit from m*, which needs appropriate
	   ;; matrix multiplication.
	   ;; Do we define these as non-oriented methods?  (i.e. with
	   ;; Nx1 and 1xN methods doing the right thing when added
	   ;; together?  Currently, we'd barf on the mis-alignment.
	   ;; and 

	   print-object
	   mref data row col
	   nelts  nrows ncols
	   matrix-dimension matrix-dimensions
	   orientation valid-orientation-p opposite-orientation
	   flatten-matrix-indices flatten-matrix-indices-1

	   la-simple-matrix-double  la-simple-matrix-integer
	   la-simple-matrix-single
	   la-simple-matrix-complex-single 
	   la-simple-matrix-complex-double
	   ;; Next  symbols are guesses at... wrong?
	   la-simple-matrix-fixnum 
	   
	   la-simple-vector-double
	   la-simple-vector-single
	   la-simple-vector-integer
	   la-simple-vector-complex-single
	   la-simple-vector-complex-double

	   ;; Next paragrah of symbols are guesses... wrong?
	   fa-simple-matrix-double  fa-simple-matrix-integer
	   fa-simple-matrix-complex  fa-simple-matrix-float 
	   fa-simple-vector-double fa-simple-vector-integer
	   fa-simple-matrix-fixnum 

	   col-vector-p
	   make-vector
	   parent
	   real-stride
	   row-vector-p

	   ;; exported BLAS/LAPACK, the "simple" versions which handle
	   ;; the various types (double, complex-float, integer,
	   ;; etc...) 
	   gemm scal
	   iamax asum nrm2 axpy slice
	   dot dotc dotu

	   ;; data storage modes and defaults.
	   *supported-datatypes* datatype->letter
	   float double complex-float complex-double
	   single-float double-float
	   *default-element-type*
	   
	   ;; actual storage place (lisp or foreign)
	   *implementations* *default-implementation*

	   make-predicate make-predicate-macro

	   assert-valid-matrix-index

	   window-matview strided-matview

	   bind2
	   
	   diagonal! diagonalf
	   ))


(defpackage :lisp-matrix-user
  (:documentation "User experimentation package for lisp-matrix")
  (:use :cl
	:lisp-matrix))

(defpackage :lisp-matrix-unittests
  (:documentation "Unit, validation, and regression testing for lisp-matrix")
  (:use :common-lisp
	:lift
	:lisp-matrix)
  (:export run-lisp-matrix-tests))
