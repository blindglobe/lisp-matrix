;;; -*- mode: lisp -*-

;;; Time-stamp: <2012-11-29 13:33:26 tony>
;;; Creation:   <2008-12-02 17:28:08 tony>
;;; File:       data-transform.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2008--, AJ Rossini.
;;; License:    MIT
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
;;; and matrices.  This is the only point of this file's functions.

;;; While any sensible lisp programmer would savor the API, I´m
;;; corrupting it in favor of getting-stuff-done.

(in-package :lisp-matrix)

;;; LIST <-> VECTOR-LIKE

(defun list->vector-like (listvar
			  &key
			  (coerce-to 'double-float)
			  (orientation :row))
  "Create a vector-like using default implementation. Use globals to
change implementation, etc.  By default, we assume lists are
variables, not cases (i.e. follow lispstat convention), and therefore
convert to column."
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


;; FIXME: this function needs to be:
;; - improved (efficiency/speed)
;; - made generic possibly with a different name/API, to support
;;   sparse trapezoid and triangular matrices, and other similar
;;   types.
;; - possible: learn to use classes to determine TYPE.
(defun trap2mat (m &key (type :upper))
  "Copy the trapezoid, lower or upper, into the other side (i.e. upper
triagular storage into full storage).  For non-square matrices, there
might be a bit of excess to ignore; but we only handle the top square
of the rectangle."
  (check-type m matrix-like)
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
 (defparameter *trap2mat-test1*
  (make-matrix 3 3 :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0))))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1*)))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1* :type :upper)))
 (assert (matrix-like-symmetric-p (trap2mat *trap2mat-test1* :type :lower)))
|#
