;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-06-24 07:52:25 tony>
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
  "This macro computes the needed workspace, and then recalls the
function with the correct-sized array (appropriately allocated).
lwork, work are the appropriate symbols, and type should be the
replaceable type from def-lapack-method."
  (let ((element-type (fnv-type->element-type type)))
    `(let ((work (make-vector 1 :element-type ',element-type
                                :implementation :foreign-array))
           (,lwork -1))
       ,call
       ;; We call twice to set lwork (first time is initial framework,
       ;; second time is actual computation).
       (setq ,lwork (floor (mref ,work 0 0)))
       (setq ,work (make-vector ,lwork :element-type ',element-type
                                :implementation :foreign-array))
       ,call)))

(defun check-info (info function-name)
  (unless (= info 0)
    (error "~a: error in argument ~d" function-name (- info))))

