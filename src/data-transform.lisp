;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-03-17 18:18:55 tony>
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

(defun list->vector-like (listvar &key
			  (coerce-to 'double-float)
			  (orientation :column))
  "Create a vector-like using default implementation. Use globals to
change implementation, etc.  By default, we assume lists are
variables, not cases, and therefore convert to column."
  (make-vector (length listvar)
	       :type orientation
	       :initial-contents
	       (ecase orientation
		 (:row
		  (list 
		   (mapcar #'(lambda (x) (coerce x coerce-to))
			   listvar)))
		 (:column
		  (mapcar #'(lambda (x) (list (coerce x coerce-to)))
			  listvar)))))

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


 (defun matrix-like->listoflists ())



 (defun matrix-like->array ())

 (defun array->matrix-like ())
|#


(defun trap2mat (m &key (type :upper))
  "Copy the trapezoid, lower or upper, into the other side (i.e. upper
triagular storage into full storage).  For non-square matrices, there
might be a bit of excess to ignore; but we only handle the top square
of the rectangle."
  (let ((mindim (reduce #'min (matrix-dimensions m)))
	(result (copy m)))
    (ecase type
      (:upper (dotimes (i mindim)
		(dotimes (j i)
		  (setf (mref result i j) (mref m j i)))))
      (:lower (dotimes (i mindim)
		(dotimes (j i)
		  (setf (mref result j i) (mref m i j))))))
    result))

#|

  (defun trap2mat (m &key (type :upper))
    "convert a upper/lower triangular storage to an actual normal but
  symmetric matrix.  
  FIXME: Current only workis for square matrices -- needs to work for
  the square-ish minimal sized square matrix within a rectangular
  matrix."
    (check-type m matrix-like)
    (let ((mc (copy m)))
      (dotimes (i (nrows m))
	(dotimes (j i)
	  (ecase type
	    (:upper (setf (mref mc i j) (mref m j i)))
	    (:lower (setf (mref mc j i) (mref m i j))))))
      mc))

 (defparameter *trap2mat-test1*
  (make-matrix 3 3 :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0))))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1*)))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1* :type :upper)))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1* :type :lower)))

|#
