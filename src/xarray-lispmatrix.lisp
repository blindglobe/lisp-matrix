;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-06-26 08:13:36 tony>
;;; Creation:   <2009-06-26 07:57:23 tony>
;;; File:       xarray-lispmatrix.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Integration with xarray for a common array-like
;;;             indexing approach.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


(in-package :lisp-matrix)

;; need to condition on the existence of and accessibility of xarray,
;; something like:

(when (find-package 'xarray)


  (defmethod xtype ((object matrix-like)))

  (defmethod xrank ((object matrix-like)))

  (defmethod xdims ((object matrix-like)))

  ;; (defmethod xdims*) can just use the default method.

  (defmethod xdim ((object matrix-like)))

  (defmethod xsize ((object matrix-like)))

  (defmethod xref-writable-p ((object matrix-like) &rest subscripts))

  (defmethod xref ((object matrix-like) &rest subscripts))

  (defmethod (setf xref) (value (object matrix-like) &rest subscripts))

  ;; there is a default method that should suffice, but perhaps some
  ;; direct tricks could make this more efficient?  Worth a look
  ;; LATER. 
#|
  (defmethod xsetf ((destination matrix-like)
		    (source matrix-like)
		    &key map-function))
|#

  ;;   (defmethod take) ; default should work for now.
  )