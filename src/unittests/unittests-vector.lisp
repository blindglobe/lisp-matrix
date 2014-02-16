;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007--2014, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is part of the unittests package.   See unittests.lisp for
;;; general philosophy.

(in-package :lisp-matrix-unittests)

;; See file:test.lisp in this directory for debugging with LIFT.

;;; TEST SUITES in file.

(deftestsuite lisp-matrix-ut-vectors      (lisp-matrix-ut) ())
(deftestsuite lisp-matrix-ut-vectors-gemm (lisp-matrix-ut-vectors) ())

;;; SUPPORT FUNCTIONS

;;; TESTS: VECTORS

(addtest (lisp-matrix-ut-vectors)
  construct-vectors-and-same-as-matrix
  (for-all-implementations
    (ensure (m= (make-vector 3 :initial-element 0d0)
		(make-matrix 1 3 :initial-element 0d0)))
    (ensure (m= (make-vector 3 :initial-element 0d0 :type :column)
		(make-matrix 3 1 :initial-element 0d0)))
    (ensure (col-vector-p (rand 3 1)))
    (ensure (row-vector-p (rand 1 3)))

    ;; FIXME: M x 1 or 1 x M matrices should not be considered
    ;; transposed when we think of their storage.  But we cannot
    ;; transpose them without resorting to a TRANSPOSE-VECVIEW.  So it
    ;; would be best to introduce a function like STORAGE-TRANSPOSED-P
    ;;   (ensure (not (transposed-p (transpose-matrix (make-matrix 1 10)))))
    ;;   (ensure (not (transposed-p (transpose-matrix (make-matrix 10 1)))))

    ;; transpose should return the original matrix if dimensions are
    ;; 1 x 1
    (let ((m (rand 1 1)))
      (ensure (eq m (transpose-matrix m))))))

(addtest (lisp-matrix-ut-vectors)
  matview-row-and-col-access-and-equiv
  (for-all-implementations
    (let ((a (rand 7 9)))
      ;; strides and window should return vectors when appropriate
      (ensure (row-vector-p (window a :nrows 1)))
      (ensure (col-vector-p (window a :ncols 1))) 
      ;; column access and row access, matviews.
      (dotimes (i 7)
	(ensure (v= (row a i) (col (transpose-matrix a) i)))
	(ensure (not (m= (row a i) (col (transpose-matrix a) i))))
	(ensure (row-vector-p (row a i)))
	(ensure (col-vector-p (col a i)))
	(ensure (row-vector-p (row (transpose-matrix a) i)))
	(ensure (col-vector-p (col (transpose-matrix a) i)))))))


(addtest (lisp-matrix-ut-vectors)
  strided-matrix-row-access
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 3 :row-stride 2)))  ;; need an indexed variant
    (ensure  (m= (row b 0) 
		 (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)))))
;;    (ensure  (m=  (princ (row b 0)  )
;;    (princ (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))) )
    (ensure (m= (row b 1)
            (make-matrix 1 5 :initial-contents '((11d0 12d0 13d0 14d0 15d0)))))
    (ensure (m= (row b 2)
            (make-matrix 1 5 :initial-contents '((21d0 22d0 23d0 24d0 25d0)))))))


(addtest (lisp-matrix-ut-vectors)
  strided-matrix-column-access
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 3 :row-stride 2)))
;;     (princ b)
;;     (ensure (m= (princ  (col b 0))
;; 		(princ  (make-matrix 3 1 :initial-contents '((1d0) (11d0) (21d0))))))
;;     (ensure (m= (princ  (col b 1))
;; 		(princ  (make-matrix 3 1 :initial-contents '((2d0) (12d0) (22d0))))))
;;     (ensure (m= (princ (col b 2) )
;; 		(princ  (make-matrix 3 1 :initial-contents '((3d0) (13d0) (23d0))))))
    (ensure (m= (col b 0)
		(make-matrix 3 1 :initial-contents '((1d0) (11d0) (21d0)))))
    (ensure (m= (col b 1)
		(make-matrix 3 1 :initial-contents '((2d0) (12d0) (22d0)))))
    (ensure (m= (col b 2)
		(make-matrix 3 1 :initial-contents '((3d0) (13d0) (23d0)))))
    (ensure (m= (col b 3)
		(make-matrix 3 1 :initial-contents '((4d0) (14d0) (24d0)))))
    (ensure (m= (col b 4)
		(make-matrix 3 1 :initial-contents '((5d0) (15d0) (25d0)))))))

(addtest (lisp-matrix-ut-vectors)
  v=-col-row-transpose
  (let ((a (rand 3 4)))
    (dotimes (i 2) 
      (ensure (v= (row a i) (col (transpose-matrix a) i)))
      (ensure (v= (col a i) (row (transpose-matrix a) i))))))

(addtest (lisp-matrix-ut-vectors)
  row-of-window
  (let* ((a (rand 5 10 :element-type 'integer :value 10))
         (b (window a :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))
    (dotimes (i 4)
      (ensure (m= (row b i)
		  (window a :row-offset (+ i 1) :nrows 1 :col-offset 2 :ncols 5)))))
  (let* ((a (rand 10 5 :element-type 'integer :value 10))
         (b (window (transpose-matrix a) :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))

    (dotimes (i 4)
    (ensure (m= (row b i)
            (window (transpose-matrix a) :row-offset (+ i 1)  :nrows 1 :col-offset 2
		    :ncols 5))))))

(addtest (lisp-matrix-ut-vectors)
  real-stride
  (ensure (= 1 (real-stride (zeros 2 2))))
  (ensure (= 2 (real-stride (row (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (col (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (row (transpose-matrix (zeros 2 2)) 0))))
  (ensure (= 2 (real-stride (col (transpose-matrix (zeros 2 2)) 0))))
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



;;; DIAGONAL CLASS TESTS

(addtest (lisp-matrix-ut-vectors)
  diagonal!-vectors
  (for-all-implementations
    (let* ((a (make-matrix 4 4
			   :initial-contents '((0d0 1d0 2d0 3d0)
					       (1d0 2d0 3d0 4d0)
					       (2d0 3d0 4d0 5d0)
					       (3d0 4d0 5d0 6d0))))
           (b (make-matrix 1 4
			   :initial-contents '((0d0 2d0 4d0 6d0))))
           (c (make-vector 4
			   :initial-contents '((0d0 2d0 4d0 6d0))
			   :type :row))
           (d (make-vector 4
			   :initial-contents '((0d0)( 2d0)( 4d0)( 6d0))
			   :type :column)))
      (ensure (m= (diagonal! a)
 		  b))
      (ensure (m= (diagonal! (transpose-matrix a))
 		  b))
      (ensure (v= (diagonal! a)
		  b))
      (ensure (v= (diagonal! (transpose-matrix a))
		  b)))))



(addtest (lisp-matrix-ut-vectors)
  diagonalf-vectors
  (for-all-implementations
    (let* ((a (make-matrix 4 4
			   :initial-contents '((0d0 1d0 2d0 3d0)
					       (1d0 2d0 3d0 4d0)
					       (2d0 3d0 4d0 5d0)
					       (3d0 4d0 5d0 6d0))))
           (b (make-matrix 1 4
			   :initial-contents '((0d0 2d0 4d0 6d0))))
           (c (make-vector 4
			   :initial-contents '((0d0 2d0 4d0 6d0))
			   :type :row))
           (d (make-vector 4
			   :initial-contents '((0d0)( 2d0)( 4d0)( 6d0))
			   :type :column)))
      (ensure (m= (diagonalf a)
 		  b))
      (ensure (m= (diagonalf (transpose-matrix a))
 		  b))
      (ensure (v= (diagonalf a)
		  b))
      (ensure (v= (diagonalf (transpose-matrix a))
		  b)))))

;;;; Vectors

(addtest (lisp-matrix-ut-vectors)
  vector-op-v+
  (for-all-implementations
    (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
           (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	   (c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0)))))
      (ensure (v= (v+ a b)
		  c))
      (ensure (v= (v+ b a)
		  c)))))

(addtest (lisp-matrix-ut-vectors)
  vector-op-v-
  (for-all-implementations
    (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
           (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	   (c (make-vector 4 :initial-contents '((-9d0 -18d0 -27d0 -36d0))))
	   (d (make-vector 4 :initial-contents '((9d0 18d0 27d0 36d0)))))
      (ensure (v= (v- a b)
		  c))
      (ensure (v= (v- b a)
		  d)))))

(addtest (lisp-matrix-ut-vectors)
  vector-op-v*
  (for-all-implementations
    (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
           (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	   (c (make-vector 4 :initial-contents '((10d0 40d0 90d0 160d0)))))
      (ensure (v= (v* a b)
		  c))
      (ensure (v= (v* b a)
		  c)))))

(addtest (lisp-matrix-ut-vectors)
  vector-op-v/
  (for-all-implementations
    (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
           (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	   (c (make-vector 4 :initial-contents '((10d0 10d0 10d0 10d0))))
	   (d (make-vector 4 :initial-contents '((0.1d0 0.1d0 0.1d0 0.1d0)))))
      (ensure (v= (v/ a b)
		  d))
      (ensure (v= (v/ b a)
		  c)))))
