;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-12-02 17:40:41 tony>
;;; Creation:   <2008-12-02 17:28:08 tony>
;;; File:       matrix-lists.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    conversion functions:
;;;                lists <-> vectors,
;;;                lists of lists <-> matrices


;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; Somewhere, we need to include some helper functions which allow us
;;; to convert between lists and vectors, and between lists of lists
;;; and matrices.  This is the point of the functions here.

(in-package :lisp-matrix)

(defun list->vector-like (listvar)
  "Create a vector-like using default implementation. Use globals to
change implementation, etc."
  (make-vector (length listvar)
	       :type :row
	       :initial-contents
	       (list 
		(mapcar #'(lambda (x) (coerce x 'double-float))
			listvar))))


(defun vector-like->list (vecvar)
  "Create a list from a vector-like."
  (let ((result (make-array (list (nelts vecvar)))))
    (dotimes (i (nelts vecvar))
      (setf (aref result i) (vref vecvar i)))
    result))


(defun matrix-like->listoflists ())

(defun listoflists->matrix-like ())
