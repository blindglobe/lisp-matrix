;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-01-03 20:00:06 tony>
;;; Creation:   <2008-12-02 17:28:08 tony>
;;; File:       data-transform.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    conversion functions:
;;;             +  lists <-> vector-like 
;;;                lists of lists <-> matrix-like
;;;                vector <-> vector-like
;;;                array <-> vector-like
;;;                array <-> matrix-like

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; Somewhere, we need to include some helper functions which allow us
;;; to convert between lists and vectors, and between lists of lists
;;; and matrices.  This is the point of the functions here.

;;; While any sensible lisp programmer would savor the API, I´m
;;; corrupting it in favor of getting-stuff-done.

(in-package :lisp-matrix)

(defun list->vector-like (listvar &key (coerce-to 'double-float))
  "Create a vector-like using default implementation. Use globals to
change implementation, etc."
  (make-vector (length listvar)
	       :type :row
	       :initial-contents
	       (list 
		;; (mapcar #'(lambda (x) (coerce x 'double-float))
		(mapcar #'(lambda (x) (coerce x coerce-to))
			listvar))))


(defun vector-like->list (vecvar)
  "Create a list from a vector-like."
  (let ((result (make-array (list (nelts vecvar)))))
    (dotimes (i (nelts vecvar))
      (setf (aref result i) (vref vecvar i)))
    result))

#| coding logic only at this point.
(defun listoflists->matrix-like (lol &optional (coerce-to 'double-float))
  (let ((dims (loop lol counting number of subunits, first number is
		 number in inside, second is nil if they don't all
		 equal each other))
	)
    (let ((result (apply #'make-matrix dims))
	  )
      (loop-over-lol counting i j and setting value, and
	   (setf (mref result i j) value)))  )  )
|#


#|
(defun matrix-like->listoflists ())



(defun matrix-like->array ())

(defun array->matrix-like ())
|#

