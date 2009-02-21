;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-02-21 14:50:58 tony>
;;; Creation:   <2009-02-05 11:18:51 tony>
;;; File:       lapack-methods.lisp
;;; Author:     Mark H. < @ >
;;; Maintainer: AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Invocation across storage types (local/foreign; precision
;;;             / real/complex)

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-matrix)

;;; This file contains actual BLAS/LAPACK method invocation from Lisp.
;;; See functions in lapack-utils.lisp for how supporting utility
;;; macros and functions work.

;;; * Blas methods
;;;
;;; ** Level 1 BLAS
;;;
;;; Done: xSCAL, xAXPY, xDOT xDOTU, xDOTC, 
;;;
;;; Miss some names: xNRM2, xASUM, IxAMAX
;;; FIXME (AJR): these look to be done?   What does the above mean??
;;;
;;; TODO: xROTG, xROTMG, xROT, xROTM, xSWAP, xCOPY, xSDOT

(def-lapack-method scal (alpha (x !matrix-type))
  (assert (typep alpha '!element-type))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p))
                   t))
      x
    (!function (nelts x) alpha x 1)))

;; FIXME: needed so to harmonize vectors with matrix cases
(defmethod scal (alpha (x la-vector-double))
  (assert (typep alpha 'double-float))
  (with-copies ((x (not real-stride) t))
      x
    (%dscal (nelts x) alpha x (real-stride x))))

#+nil
(let ((x (ones 5 5)))
  (time
   (progn
     (scal 2d0 (row x 1))
     (scal 3d0 (col x 3))
     x)))

(def-lapack-method axpy (alpha (x !matrix-type) (y !matrix-type))
  (assert (typep alpha '!element-type))
  (assert (= (nelts x) (nelts y)))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p)))
                (y (or (not unit-strides-p)
                       (not zero-offset-p))
                   t))
      y
   (!function (nelts x) alpha x 1 y 1)))

(def-lapack-method dot ((x !matrix-type) (y !matrix-type))
  (assert (= (nelts x) (nelts y)))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p)))
                (y (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    (!function (nelts x) x 1 y 1)))

(def-lapack-method dotu ((x !matrix-type) (y !matrix-type))
  (assert (= (nelts x) (nelts y)))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p)))
                (y (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    (!function (nelts x) x 1 y 1)))

(def-lapack-method dotc ((x !matrix-type) (y !matrix-type))
  (assert (= (nelts x) (nelts y)))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p)))
                (y (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    (!function (nelts x) x 1 y 1)))

(def-lapack-method (nrm2 :function-names
                         ((%snrm2 single-float)
                          (%dnrm2 double-float)
                          (%scnrm2 (complex single-float))
                          (%dznrm2 (complex double-float))))
    ((x !matrix-type))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    (!function (nelts x) x 1)))

(def-lapack-method (asum :function-names
                         ((%sasum single-float)
                          (%dasum double-float)
                          (%scasum (complex single-float))
                          (%dzasum (complex double-float))))
    ((x !matrix-type))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    (!function (nelts x) x 1)))

(def-lapack-method (iamax :function-names
                          ((%isamax single-float)
                           (%idamax double-float)
                           (%icamax (complex single-float))
                           (%izamax (complex double-float))))
    ((x !matrix-type))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p))))
      nil
    ;; LAPACK element numbering starts from 1, so we correct this to
    ;; the lisp style starting from 0.
    (1- (!function (nelts x) x 1))))

;;; ** Level 2 BLAS
;;;
;;; Done: none
;;; 
;;; To do: xGEMV, xGBMV, xHBMV, xHPMV, xSYMV, xSBMV, xSPMV, xTRMV,
;;; xTBMV, xTPMV, xTRSV, xTBSV, xTPSV, xGER, xGERU, xGERC, xHER,
;;; xHPR, xHER2, xHPR2, xSYR, xSPR, xSYR2, xSPR2
;;;
;;; ** Extended precision Level 2 BLAS
;;;
;;; Done:
;;;
;;; To do:
;;; 
;;; ** LEVEL 3 BLAS
;;;
;;; Done: xGEMM
;;;
;;; To do: xSYMM, xHEMM, xSYRK, xHERK, xSYR2K, xHER2K, xTRMM, xTRSM,

(def-lapack-method gemm (alpha
			 (a !matrix-type)
			 (b !matrix-type)
			 beta
			 (c !matrix-type))
  (assert (= (ncols a) (nrows b)))
  (assert (= (nrows a) (nrows c)))
  (assert (= (ncols b) (ncols c)))
  (with-copies ((a (or (not unit-strides-p)))
                (b (or (not unit-strides-p)))
                (c (or (not unit-strides-p)
                       transposed-p)
                   t))
      c
    (!function (orientation-letter a)
               (orientation-letter b)
               (nrows a)
               (ncols b)
               (ncols a)
               alpha
               a
               (real-nrows a)
               b
               (real-nrows b)
               beta
               c
               (real-nrows c))))

;;; * Lapack
;;;
;;; Done:
;;;
;;; Need more work:
;;;   xGELSY (no complex support).
;;;   xPOTRF (incomplete, no tests)
;;;   xPOTRI (incomplete, no tests)
;;;   xGEQRF (incomplete, no tests)
;;; TODO: many many

(defmacro call-with-work ((lwork work type) call)
  (let ((element-type (fnv-type->element-type type)))
    `(let ((work (make-vector 1 :element-type ',element-type
                                :implementation :foreign-array))
           (,lwork -1))
       ,call
       ;; WHY DO WE CALL TWICE, above?  JUST TO SET LWORK? OR CHECK
       ;; INFO? Looks like to set LWORK (first time is initial
       ;; framework, second time is actual computation).
       (setq ,lwork (floor (mref ,work 0 0)))
       (setq ,work (make-vector ,lwork :element-type ',element-type
                                :implementation :foreign-array))
       ,call)))

(defun check-info (info function-name)
  (unless (= info 0)
    (error "~a: error in argument ~d" function-name (- info))))

;;; Solving ax = b
;;; note that 'a' will be modified upon the call, and will have a
;;; different value at the end, more appropriate to the transformation
;;; (i.e. will be the QR, but stored in common compact form).
(def-lapack-method gelsy ((a !matrix-type)
			  (b !matrix-type)
                          rcond &optional jpvt)
  ;; FIXME: has both LWORK and RWORK for %ZGELSY and %CGELSY
  (unless jpvt
    (setq jpvt (make-fnv-int32 (ncols a) :initial-value 0)))
  (let ((rank (make-fnv-int32 1 :initial-value 0))
        (info (make-fnv-int32 1 :initial-value 0)))
    ;; FIXME: B needs to be resized anyway if A has more columns than
    ;; rows, to allow for enough storage for the result matrix =>
    ;; quick fix: disallow this
    (assert (<= (ncols a) (nrows a)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p))
                  (b (or (not unit-strides-p)
                         transposed-p)
		     t))
        ;; FIXME: the value RANK is not correct because the cffi type
        ;; :FORTRAN-INT does not define a TRANSLATE-FROM-FOREIGN method?
        ;; => why not use a standard cffi integer type anyway??
        ;; (a fix is to make RANK a fnv-int32 with one element
        (progn
          (check-info (fnv-int32-ref info 0) "GELSY")
          (list (if (= (nrows b) (ncols a)) ; returns (list b rank)
                    b
                    (window b :nrows (ncols a)))
                (fnv-int32-ref rank 0)))
      (call-with-work (lwork work !data-type)
		      (!function (nrows a)
				 (ncols a)
				 (ncols b)
				 a
				 (real-nrows a)
				 b
				 (real-nrows b)
				 jpvt
				 rcond
				 rank
				 (data work)
				 lwork
				 info)))))

#+nil
(setf *temp-result* 
      (let ((*default-implementation* :foreign-array))
	(let* ((m 10)
	       (n 10)
         (a (rand m n))
         (x (rand n 1))
         (b (m* a x))
         (rcond (* (coerce (expt 2 -52) 'double-float)
                   (max (nrows a) (ncols a))))
         (orig-a (copy a))
         (orig-b (copy b))
         (orig-x (copy x)))
    (list x (gelsy a b rcond)))))

#+nil
(setf *temp-result* 
      (let ((*default-implementation* :lisp-array))
	(let* ((m 10)
	       (n 10)
         (a (rand m n))
         (x (rand n 1))
         (b (m* a x))
         (rcond (* (coerce (expt 2 -52) 'double-float)
                   (max (nrows a) (ncols a))))
         (orig-a (copy a))
         (orig-b (copy b))
         (orig-x (copy x)))
    (list x (gelsy a b rcond)))))



;;; LU

;;; GETRF - compute the LU Factorization of a matrix.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method getrf ((a !matrix-type))
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
    (unless ipvt
      (setf ipvt (make-fnv-int32 (ncols a) :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p) 
		     t))
      (list a
	    (check-info (fnv-int32-ref info 0) "GETRF"))
      (call-with-work  (lwork work !data-type)
		       (!function (nrows a)        ; N 
				  (ncols a)        ; M
				  a                ; A
				  (max 1 (ncols a)); LDA
				  ipiv
;;        IPIV    (output) INTEGER array, dimension (min(M,N))
;;                The  pivot  indices;  for 1 <= i <= min(M,N), row i of
;; 		  the matrix was interchanged with row IPIV(i).
				  info))))); info

;;; POTRI - compute the inverse of a real symmetric positive definite
;;; matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
(def-lapack-method potri ((a !matrix-type))
  (assert (= (ncols a) (nrows a)))  ;; only works with square matrices
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)
		     t))
	;; Returning:
	;; - inverse,
	;; - "U" since upper format trangular,
	;; - info, for correctness of results.  
	;; Should we put INFO first?!
	(list a
	      "U" ; not useful until we add option for lowercase.
	      (check-info (fnv-int32-ref info 0) "POTRI"))
      (!function "U"        ; "L" (in) is lower an option? 
		 (ncols a)  ; N (in) (order of matrix, columns, 2nd index
		 a          ; a (in/out) matrix
		 (nrows a)  ; LDA (in) leading dimension, LDA >= max(1,N)
		            ; above was "(real-nrows a)" ?
		 info))))   ; info (out)




;;; CHOLESKY

;;; POTRF - compute the Cholesky Factorization of a real sym pos-def
;;; matrix A.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method potrf ((a !matrix-type))
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p) 
		     t))
      (list a
	    "U"
	    (check-info (fnv-int32-ref info 0) "POTRF"))
      (!function "U"  ; store in Upper section
		 (ncols a) ; N 
		 a
		 (real-nrows a) ; LDA
		 info)))) ; info

;;; POTRI - compute the inverse of a real symmetric positive definite
;;; matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
(def-lapack-method potri ((a !matrix-type))
  (assert (= (ncols a) (nrows a)))  ;; only works with square matrices
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)
		     t))
	;; Returning:
	;; - inverse,
	;; - "U" since upper format trangular,
	;; - info, for correctness of results.  
	;; Should we put INFO first?!
	(list a
	      "U" ; not useful until we add option for lowercase.
	      (check-info (fnv-int32-ref info 0) "POTRI"))
      (!function "U"        ; "L" (in) is lower an option? 
		 (ncols a)  ; N (in) (order of matrix, columns, 2nd index
		 a          ; a (in/out) matrix
		 (nrows a)  ; LDA (in) leading dimension, LDA >= max(1,N)
		            ; above was "(real-nrows a)" ?
		 info))))   ; info (out)

;;; QR decomposition.  Need one more front end to provide appropriate
;;; processing.  A and TAU will have different values at the end, more
;;; appropriate to the transformation (i.e. will be the QR, but stored
;;; in common compact form).
;;;
;;        M       (input) INTEGER
;;                The number of rows of the matrix A.  M >= 0.
;;        N       (input) INTEGER
;;                The number of columns of the matrix A.  N >= 0.
;;        A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
;;                On entry, the M-by-N matrix A.  On exit, the  elements  on  and
;;                above the diagonal of the array contain the min(M,N)-by-N upper
;;                trapezoidal matrix R (R is upper triangular if  m  >=  n);  the
;;                elements  below the diagonal, with the array TAU, represent the
;;                orthogonal matrix Q as a product of min(m,n) elementary reflec‐
;;                tors (see Further Details).
;;        LDA     (input) INTEGER
;;                The leading dimension of the array A.  LDA >= max(1,M).
;;        TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
;;                The  scalar  factors  of the elementary reflectors (see Further
;;                Details).
;;        WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
;;                On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
;;        LWORK   (input) INTEGER
;;                The dimension of the array WORK.  LWORK >= max(1,N).  For opti‐
;;                mum  performance  LWORK >= N*NB, where NB is the optimal block‐
;;                size.
;;                If LWORK = -1, then a workspace query is assumed;  the  routine
;;                only  calculates  the  optimal  size of the WORK array, returns
;;                this value as the first entry of the WORK array, and  no  error
;;                message related to LWORK is issued by XERBLA.
;;        INFO    (output) INTEGER
;;                = 0:  successful exit
;;                < 0:  if INFO = -i, the i-th argument had an illegal value

(def-lapack-method geqrf ((a !matrix-type)
			  (tau !matrix-type)) ; tau is for output
  (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)
		     t)
                  (tau (or (not unit-strides-p)
			   transposed-p)
		       t))
	(list a
	      tau
	      (check-info (fnv-int32-ref info 0) "GEQRF"))
      (call-with-work (lwork work !data-type)
		      (!function (nrows a)
				 (ncols a)
				 a
				 (max 1 (nrows a))
				 (data tau)
				 (data work)
				 lwork
				 info)))))
