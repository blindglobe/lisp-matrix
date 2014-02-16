;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007--2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is part of the unittests package.   See unittests.lisp for
;;; general philosophy.

(in-package :lisp-matrix-unittests)

;; See file:test.lisp in this directory for debugging with LIFT.

;;; TEST SUITES

(deftestsuite lisp-matrix-ut-datatrans      (lisp-matrix-ut) ())

;;; SUPPORT FUNCTIONS

;;; TESTS: VECTORS

(addtest (lisp-matrix-ut-datatrans)
  list-to-vector-like-row
  (for-all-implementations
    (ensure (m= (make-vector 3
			     :initial-element 0d0
			     :type :row )
		(list->vector-like (list 0d0 0d0 0d0)
				   :orientation :row)))))

(addtest (lisp-matrix-ut-datatrans)
  list-to-vector-like-column
  (for-all-implementations
    (ensure (m= (make-vector 3
			     :initial-element 0d0
			     :type :column )
		(list->vector-like (list 0d0 0d0 0d0)
				   :orientation :column)))))

