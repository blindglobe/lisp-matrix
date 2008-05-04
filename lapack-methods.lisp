(in-package :lisp-matrix)

;;; This file contains actual LAPACK methods.  See functions in
;;; lapack-utils.lisp for how supporting utility macros and functions.
;;;
;;; Time-stamp: <2008-05-04 13:29:49 Evan Monroig>

(def-lapack-method gemm (alpha (a !matrix-type) (b !matrix-type)
                               &optional (beta 0d0) c)
  (assert (= (ncols a) (nrows b)))
  (unless c
    (setq c (make-matrix (nrows a) (ncols b) '!data-type)))
  (check-type c !matrix-type)
  (assert (= (nrows a) (nrows c)))
  (assert (= (ncols b) (ncols c)))
  (with-copies ((a (or (not unit-stride-p)
                       (not zero-offset-p)))
                (b (or (not unit-stride-p)
                       (not zero-offset-p)))
                (c (or (not unit-stride-p)
                       (not zero-offset-p)
                       transposed-p)
                   t))
      c
    (!function (orientation-letter a)
               (orientation-letter b)
               (nrows a)
               (ncols b)
               (ncols a)
               alpha
               (data a)
               (real-nrows a)
               (data b)
               (real-nrows b)
               beta
               (data c)
               (real-nrows c))))
