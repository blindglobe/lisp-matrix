;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lisp-matrix core package.  The dependency
;;; should be that lisp-matrix packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :lisp-matrix-unittests)

;; See file:test.lisp in this directory for debugging with LIFT.

;;; EXTERNAL

;;; TEST SUITES

(deftestsuite lisp-matrix-ut-matrix-views  (lisp-matrix-ut-matrix) ())

;;; SUPPORT FUNCTIONS

;;; TESTS: MATRIX-VIEWS

(addtest (lisp-matrix-ut-matrix-views)
  fun-transpose
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eq a (transpose-matrix (transpose-matrix a)))))))

(addtest (lisp-matrix-ut-matrix-views)
  fun-window
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eq a (parent (window (window a :ncols 2)
                                :nrows 2))))
      (ensure (m= (window (window a :ncols 2) :nrows 2)
              (window a :ncols 2 :nrows 2))))))

(addtest (lisp-matrix-ut-matrix-views)
  fun-strides
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eql (class-name (class-of (strides a :nrows 2)))
               (window-class a)))
      (ensure (eq a (parent (strides (strides a :ncols 2 :col-stride 2))))))))


(addtest (lisp-matrix-ut-matrix-views)
  indexing-views
  (let* ((m3 (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						  (6d0  7d0  8d0  9d0  10d0)
						  (11d0 12d0 13d0 14d0 15d0)
						  (16d0 17d0 18d0 19d0 20d0)
						  (21d0 22d0 23d0 24d0 25d0)
						  (26d0 27d0 28d0 29d0 30d0))))
	 (m4 (strides m3 :nrows 2 :row-stride 2)))
    (ensure (v= (row m4 1)
		(col (transpose-matrix m4) 1) ))
    (ensure (v= (col m3 1) (row (transpose-matrix m3) 1)) )
    (ensure (v= (row m3 1) (col (transpose-matrix m3) 1)))))

