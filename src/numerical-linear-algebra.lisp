;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-02-05 13:21:26 tony>
;;; Creation:   <2009-02-05 11:18:51 tony>
;;; File:       numerical.linear.algebra.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Lispy interface to factorization/decomposition,

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :lisp-matrix)


;;; Matrix decomposition for stable processing

(defgeneric decompose (a &key type)
  (:documentation "matrix decomposition")
  (:method ((a matrix-like) type)
    (ecase type
      (:qr )
      (:ld )
      (:cholesky)
      (:svd))
    ;; (package-up-results-and-return)
    ))

(defun qr-decomp (a) "geqrf" )
(defun ld-decomp (a)  )
(defun svd-decomp (a)  )
(defun chol-decomp (a) "dpotrf"  )

;;; inversion based on factorization

(defgeneric invert (a)
  (:documentation "invert A using the appropriate factorization."))

(defun qr-invert (a)  )
(defun ld-invert (a)  )
(defun svdr-invert (a)  )
(defun chol-invert (a) "dpotri"  )

;;; [W|G]LS solutions

;; gelsy
;; gels

;;; Eigensystems


;;;

