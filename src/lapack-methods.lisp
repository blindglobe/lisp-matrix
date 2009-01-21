(in-package :lisp-matrix)

;;; This file contains actual BLAS/LAPACK method invocation from Lisp.
;;; See functions in lapack-utils.lisp for how supporting utility
;;; macros and functions work.
;;;
;;; Time-stamp: <2009-01-21 08:06:09 tony>

;;;; * Blas methods
;;;;
;;;; ** Level 1 BLAS
;;;;
;;;; Done: xSCAL, xAXPY, xDOT xDOTU, xDOTC, 
;;;;
;;;; Miss some names: xNRM2, xASUM, IxAMAX
;;;; FIXME (AJR): these look to be done?   What does the above mean??
;;;;
;;;; TODO: xROTG, xROTMG, xROT, xROTM, xSWAP, xCOPY, xSDOT

(def-lapack-method scal (alpha (x !matrix-type))
  (assert (typep alpha '!element-type))
  (with-copies ((x (or (not unit-strides-p)
                       (not zero-offset-p))
                   t))
      x
    (!function (nelts x) alpha x 1)))

;; FIXME: needed so to harmonize with matrix cases
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

;;;; ** Level 2 BLAS
;;;;
;;;; xGEMV, xGBMV, xHBMV, xHPMV, xSYMV, xSBMV, xSPMV, xTRMV, xTBMV,
;;;; xTPMV, xTRSV, xTBSV, xTPSV, xGER, xGERU, xGERC, xHER, xHPR,
;;;; xHER2, xHPR2, xSYR, xSPR, xSYR2, xSPR2
;;;;
;;;; ** Extended precision Level 2 BLAS
;;;;
;;;; ** LEVEL 3 BLAS
;;;;
;;;; Done: xGEMM
;;;;
;;;; xSYMM, xHEMM, xSYRK, xHERK, xSYR2K, xHER2K, xTRMM, xTRSM, 

(def-lapack-method gemm (alpha (a !matrix-type) (b !matrix-type) beta
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

;;;; * Lapack
;;;;
;;;; Done:
;;;;
;;;; Need more work: xGELSY (current only supports 1d p).
;;;;
;;;; TODO: many many

(defmacro call-with-work ((lwork work type) call)
  (let ((element-type (fnv-type->element-type type)))
    `(let ((work (make-vector 1 :element-type ',element-type
                                :implementation :foreign-array))
           (,lwork -1))
       ,call
       (setq ,lwork (floor (mref ,work 0 0)))
       (setq ,work (make-vector ,lwork :element-type ',element-type
                                :implementation :foreign-array))
       ,call)))

(defun check-info (info function-name)
  (unless (= info 0)
    (error "~a: error in argument ~d" function-name (- info))))

(def-lapack-method gelsy ((a !matrix-type) (b !matrix-type)
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
                         transposed-p)))
        ;; FIXME: the value RANK is not correct because the cffi type
        ;; :FORTRAN-INT does not define a TRANSLATE-FROM-FOREIGN method?
        ;; => why not use a standard cffi integer type anyway??
        ;; (a fix is to make RANK a fnv-int32 with one element
        (progn
          (check-info (fnv-int32-ref info 0) "GELSY")
          (list (if (= (nrows b) (ncols a))
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
;; (princ *temp-result*)
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
;; (princ *temp-result*)


