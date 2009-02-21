;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-02-21 16:24:42 tony>
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

;;; Matrix Factorization for stable processing

(defgeneric factor (a &key type)
  (:documentation "matrix decomposition, M -> SVD/LU/AtA etc")
  (:method ((a matrix-like) &key (type :qr))
    (let (results  (ecase type
		     (:qr (geqrf a) )
		     (:lu ( a))
		     (:cholesky (potrf a))
		     (:svd (gesvf a))) ))
    results))


;;; inversion based on factorization

(defgeneric invert (a)
  (:documentation "compute inverse of A using the appropriate factorization.")
  (:method ((a factored-matrix))
    (let (results  (ecase (factor-type a)
		     (:qr (geqri a) )
		     (:lu ( a))
		     (:cholesky (potri a))
		     (:svd (gesvi a))
		     (:otherwise (error
				  "No LAPACK/BLAS method for inversion of class ~S"
				  (factor-type a))))))
    results))


;;; [W|G]LS solutions

;; gelsy
;; gels

(defgeneric least-squares (y x &key w)
  (:documentation "Compute the (weighted/generalized) least-squares solution B to W(Y-XB)")
  (:method ((y vector-like) (x matrix-like) &key (w matrix-like) )
    (error "implement me!")))

;;; Eigensystems

(defgeneric eigensystems (x)
  (:documentation "Compute the eigenvectors and values of X.")
  (:method ((x matrix-like))
    (error "implement me!")))


;;; Optimization: should we put this someowhere else?  It is similar
;;; to Least Squares, which is one method for optimization, but is
;;; also similar to root-finding

(defgeneric optimize (f data params &key method maximize-p)
  (:documentation "given a function F, F(DATA,PARAMS), compute the
  PARAM values that optimize F for DATA, using METHOD, and maximize or
  minimize according to MAXIMIZE-P.")
  (:method ((f function) (data matrix-like) (params vector-like)
	    &key method maximize-p)
    (error "implement me!"))
  (:method ((f function) (data array) (params vector)
	    &key method maximize-p)
    (error "implement me!")))

(defgeneric root-find (f data params &key method)
  (:documentation "given a function F, F(DATA,PARAMS), compute PARAM
  such that with DATA, we use METHOD to solve F(DATA,PARAM)=0.")
  (:method ((f function) (data matrix-like) (params vector-like)
	    &key method)
    (error "implement me!"))
  (:method ((f function) (data array) (params vector)
	    &key method)
    (error "implement me!")))
