;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007--2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is part of the unittests package.   See unittests.lisp for
;;; general philosophy.

;; (asdf:oos 'asdf:compile-op 'lift :force t)
;; (asdf:oos 'asdf:load-op 'lift)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

;; EVERYTHING
;; (run-lisp-matrix-tests)
;; (describe (run-lisp-matrix-tests))

;; VECTOR TESTS
;; (run-tests :suite 'lisp-matrix-ut-vectors)
;; (describe (run-tests :suite 'lisp-matrix-ut-vectors))

;; REMINDER IF NEEDED
;; (remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

;;; TEST SUITES in file.

(deftestsuite lisp-matrix-ut-vectors      (lisp-matrix-ut) ())
(deftestsuite lisp-matrix-ut-vectors-gemm (lisp-matrix-ut-vectors) ())

;;; SUPPORT FUNCTIONS

;; (see unittests.lisp)

;;; TESTS: VECTORS

(addtest (lisp-matrix-ut-vectors)
  construct-vectors
  (for-all-implementations
    (ensure (m= (make-vector 3 :initial-element 0d0)
		(make-matrix 1 3 :initial-element 0d0)))
    (ensure (m= (make-vector 3 :initial-element 0d0 :type :column)
		(make-matrix 3 1 :initial-element 0d0)))
    (ensure (col-vector-p (rand 3 1)))
    (ensure (row-vector-p (rand 1 3)))
    (let ((a (rand 3 5)))
      (ensure (v= (row a 0) (col (transpose a) 0)))
      (ensure (not (m= (row a 0) (col (transpose a) 0))))
      (ensure (row-vector-p (row a 0)))
      (ensure (col-vector-p (col a 0)))
      (ensure (row-vector-p (row (transpose a) 0)))
      (ensure (col-vector-p (col (transpose a) 0)))
      ;; strides and window should return vectors when appropriate
      (ensure (row-vector-p (window a :nrows 1)))
      (ensure (col-vector-p (window a :ncols 1)))
      ;; transpose should return the original matrix if dimensions are
      ;; 1 x 1
      (let ((m (rand 1 1)))
        (ensure (eq m (transpose m))))
      ;; FIXME: M x 1 or 1 x M matrices should not be considered
      ;; transposed when we think of their storage.  But we cannot
      ;; transpose them without resorting to a TRANSPOSE-VECVIEW.  So
      ;; it would be best to introduce a function like
      ;; STORAGE-TRANSPOSED-P.
      ;; (ensure (not (transposed-p (transpose (make-matrix 1 10)))))
      ;; (ensure (not (transposed-p (transpose (make-matrix 10 1)))))
      )))

(addtest (lisp-matrix-ut-vectors)
  row-of-strided-matrix
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 2 :row-stride 2)))
    (ensure (m= (row b 0)
            (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)))))
    (ensure (m= (row b 1)
            (make-matrix 1 5 :initial-contents '((11d0 12d0 13d0 14d0 15d0)))))))

(addtest (lisp-matrix-ut-vectors)
  col-of-strided-matrix
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 2 :row-stride 2)))
    (ensure (m= (col b 0)
		(make-matrix 2 1 :initial-contents '((1d0) (11d0)))))
    (ensure (m= (col b 1)
		(make-matrix 2 1 :initial-contents '((2d0) (12d0)))))
    (ensure (m= (col b 2)
		(make-matrix 2 1 :initial-contents '((3d0) (13d0)))))
    (ensure (m= (col b 3)
		(make-matrix 2 1 :initial-contents '((4d0) (14d0)))))
    (ensure (m= (col b 4)
		(make-matrix 2 1 :initial-contents '((5d0) (15d0)))))))

(addtest (lisp-matrix-ut-vectors)
  v=-col-row-transpose
  (let ((a (rand 3 4)))
    ;; FIXME: this also tests ROW, COL, and their use on a transposed
    ;; matrix
    (ensure (v= (row a 0) (col (transpose a) 0)))
    (ensure (v= (col a 0) (row (transpose a) 0)))
    (ensure (v= (row a 1) (col (transpose a) 1)))
    (ensure (v= (col a 1) (row (transpose a) 1)))
    (ensure (v= (row a 2) (col (transpose a) 2)))
    (ensure (v= (col a 2) (row (transpose a) 2)))))

(addtest (lisp-matrix-ut-vectors)
  row-of-window
  (let* ((a (rand 5 10 :element-type 'integer :value 10))
         (b (window a :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 0)
            (window a :row-offset 1 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 1)
            (window a :row-offset 2 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 2)
            (window a :row-offset 3 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 3)
            (window a :row-offset 4 :nrows 1 :col-offset 2 :ncols 5))))
  (let* ((a (rand 10 5 :element-type 'integer :value 10))
         (b (window (transpose a) :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 0)
            (window (transpose a) :row-offset 1 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 1)
            (window (transpose a) :row-offset 2 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 2)
            (window (transpose a) :row-offset 3 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 3)
            (window (transpose a) :row-offset 4 :nrows 1 :col-offset 2
                                                         :ncols 5)))))

(addtest (lisp-matrix-ut-vectors)
  real-stride
  (ensure (= 1 (real-stride (zeros 2 2))))
  (ensure (= 2 (real-stride (row (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (col (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (row (transpose (zeros 2 2)) 0))))
  (ensure (= 2 (real-stride (col (transpose (zeros 2 2)) 0))))
  (ensure (null (real-stride (window (zeros 4 4) :nrows 2)))))


(addtest (lisp-matrix-ut-vectors-gemm)
  m*-vectors
  (for-all-implementations
    (let* ((a (make-matrix 4 4 :initial-contents '((0d0 1d0 2d0 3d0)
                                                   (1d0 2d0 3d0 4d0)
                                                   (2d0 3d0 4d0 5d0)
                                                   (3d0 4d0 5d0 6d0))))
           (x (slice (col a 3) :stride 2 :nelts 2 :type :row))
           (y (slice (col a 2) :stride 2 :nelts 2 :type :column)))
      (ensure (m= x (make-matrix 1 2 :initial-contents '((3d0 5d0)))))
      (ensure (m= y (make-matrix 2 1 :initial-contents '((2d0) (4d0)))))
      (ensure (m= (m* x y) (scal 26d0 (ones 1 1))))
      (ensure (m= (m* y x) (make-matrix 2 2 :initial-contents '((6d0 10d0)
                                                         (12d0 20d0))))))
    (ensure (m= (m* (ones 1 10) (ones 10 1))
            (scal 10d0 (ones 1 1))))
    (ensure (m= (m* (ones 10 1)
                (scal 2d0 (ones 1 10)))
            (scal 2d0 (ones 10 10))))))
