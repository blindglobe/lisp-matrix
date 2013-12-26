;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-12-26 13:13:00 tony>
;;; Creation:   <2009-06-26 07:57:23 tony>
;;; File:       xarray-lispmatrix.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Integration with xarray to provide a common CLOS-based
;;;             array-like indexing approach.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package :lisp-matrix)

;; might need to condition on the existence of and accessibility of xarray,
;; something like:
#|
 (when (find-package 'xarray)

  (defmethod xtype ((object matrix-like)))
  .... )
|#

(defmethod xtype ((object matrix-like))
  (warning "Not implemented for matrix-like virtual class"))

(defmethod xrank ((object matrix-like))
  (warning "Not implemented for matrix-like virtual class"))


(defmethod xdims ((object matrix-like))
  (warning "Not implemented for matrix-like virtual class"))
;; (defmethod xdims*) can just use the default method.
(defmethod xdim ((object matrix-like))
  (warning "Not implemented for matrix-like virtual class"))
(defmethod xsize ((object matrix-like))
  (warning "Not implemented for matrix-like virtual class"))
(defmethod xref-writable-p ((object matrix-like) &rest subscripts)
  "Always true for matrix-like derived classes at this point (right?)"
  t)
(defmethod xref ((object matrix-like) &rest subscripts)
  (mref object subscripts))

(defmethod (setf xref) (value (object matrix-like) &rest subscripts))


;; there is a default method that should suffice, but perhaps some
;; direct tricks could make this more efficient?  Worth a look
;; LATER. 
(defmethod xsetf ((destination matrix-like)
		  (source matrix-like)
		  &key map-function))

;; (defmethod take) ; default should work for now.



