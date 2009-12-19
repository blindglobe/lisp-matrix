;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-19 17:14:08 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       unittests-matrix-lapack.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007--, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    Matrix/LAPACK unit-tests.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


(in-package :lisp-matrix-unittests)


;;; This is semi-external to lisp-matrix core package.  The dependency
;;; should be that lisp-matrix packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be determined.

;; (asdf:oos 'asdf:compile-op 'lift :force t)
;; (asdf:oos 'asdf:load-op 'lift)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

;;(run-lisp-matrix-tests)
;;(describe (run-lisp-matrix-tests))

;;(remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

;;; EXTERNAL

;;; TEST SUITES

(deftestsuite lisp-matrix-ut-matrix-lapack (lisp-matrix-ut-matrix) ())
(deftestsuite lisp-matrix-ut-matrix-gemm   (lisp-matrix-ut-matrix) ())

;;; SUPPORT FUNCTIONS


;;; TESTS


(addtest (lisp-matrix-ut-matrix-lapack)
  datatypes
  (ensure (string= (datatype->letter 'float) "S"))
  (ensure (string= (datatype->letter 'double) "D"))
  (ensure (string= (datatype->letter 'complex-float) "C"))
  (ensure (string= (datatype->letter 'complex-double) "Z")))
;; potential export issue...  fixed, but be careful, the following explains...!
;;   (ensure-error (string= (datatype->letter 'double) "D"))
;;   (ensure (string= (datatype->letter 'lisp-matrix::double) "D"))
;;   (ensure (string= (datatype->letter 'lisp-matrix::complex-float) "C"))
;;   (ensure (string= (datatype->letter 'lisp-matrix::complex-double) "Z"))
;;   (ensure-error (string= (datatype->letter 'complex-float) "C"))
;;   (ensure-error (string= (datatype->letter 'complex-double) "Z"))
;;   (ensure (string= (datatype->letter 'complex-double) "Z"))



;; FIXME: tests below up to IAMAX fail on SBCL versions before and
;; including 1.0.11, but succeed after and including 1.0.12

(addtest (lisp-matrix-ut-matrix-lapack)
	 scal
  (for-all-implementations
    (ensure
     (m=
      (scal 1.5d0 (ones 2 2 :element-type 'double-float))
      (make-matrix 2 2 :element-type 'double-float
                       :initial-element 1.5d0)))
    (ensure
     (m=
      (scal 1.5 (ones 2 2 :element-type 'single-float))
      (make-matrix 2 2 :element-type 'single-float
                       :initial-element 1.5)))
    (ensure
     (m=
      (scal #C(1.5 1.5)
            (ones 2 2 :element-type '(complex single-float)))
      (make-matrix 2 2 :element-type '(complex single-float)
                       :initial-element #C(1.5 1.5))))
    (ensure
     (m=
      (scal #C(1.5d0 1.5d0)
            (ones 2 2 :element-type '(complex double-float)))
      (make-matrix 2 2 :element-type '(complex double-float)
                       :initial-element #C(1.5d0 1.5d0))))))

(addtest (lisp-matrix-ut-matrix-lapack)
	 axpy
	 (for-all-implementations
    (let ((*default-element-type* 'single-float))
      (ensure (m= (axpy 1.0 (ones 2 2) (scal 1.5 (ones 2 2)))
              (scal 2.5 (ones 2 2))))
      (ensure (m= (axpy -1.0 (ones 2 2) (scal 1.5 (ones 2 2)))
              (scal 0.5 (ones 2 2)))))
    (let ((*default-element-type* 'double-float))
      (ensure (m= (axpy 1d0 (ones 2 2) (scal 1.5d0 (ones 2 2)))
              (scal 2.5d0 (ones 2 2))))
      (ensure (m= (axpy -1d0 (ones 2 2) (scal 1.5d0 (ones 2 2)))
              (scal 0.5d0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float)))
      (ensure (m= (axpy #C(1.0 0.0)
                    (ones 2 2)
                    (scal #C(1.5 0.0) (ones 2 2)))
              (scal #C(2.5 0.0) (ones 2 2))))
      (ensure (m= (axpy #C(-1.0 0.0)
                    (ones 2 2)
                    (scal #C(1.5 0.0) (ones 2 2)))
              (scal #C(0.5 0.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float)))
      (ensure (m= (axpy #C(1.0d0 0.0d0)
                    (ones 2 2)
                    (scal #C(1.5d0 0.0d0) (ones 2 2)))
              (scal #C(2.5d0 0.0d0) (ones 2 2))))
      (ensure (m= (axpy #C(-1.0d0 0.0d0)
                    (ones 2 2)
                    (scal #C(1.5d0 0.0d0) (ones 2 2)))
              (scal #C(0.5d0 0.0d0) (ones 2 2)))))))


(addtest (lisp-matrix-ut-matrix-lapack)
	 dot
  (for-all-implementations
    (ensure (= (dot (ones 2 2)
		    (scal 0.5d0 (ones 2 2)))
	       2d0))
    (ensure (= (dot (ones 2 2 :element-type 'single-float)
		    (scal 0.5 (ones 2 2 :element-type 'single-float)))
	       2.0))))

#|
;; FIXME: test DOTU, DOTC
#+(or)
(addtest (lisp-matrix-ut) dotu
 (ensure (= (dotu (ones 2 2 :element-type '(complex single-float))
              (scal #C(0.5 0.0) (ones 2 2 :element-type
                                      '(complex single-float))))
        #C(2.0 0.0))))

#+(or)
(addtest (lisp-matrix-ut) dotc
 (ensure (= (dotc (ones 2 2 :element-type '(complex single-float))
              (scal #C(0.5 0.0) (ones 2 2 :element-type
                                      '(complex single-float))))
        #C(2.0 0.0))))
|#

(addtest (lisp-matrix-ut-matrix-lapack) nrm2
  (for-all-implementations
    (ensure (= (nrm2 (ones 2 2))
           2d0))
    (ensure (= (nrm2 (ones 2 2 :element-type 'single-float))
           2.0))
    (ensure (= (nrm2 (ones 2 2 :element-type '(complex single-float)))
           #C(2.0 0.0)))
    (ensure (= (nrm2 (ones 2 2 :element-type '(complex double-float)))
           #C(2d0 0d0)))))

(addtest (lisp-matrix-ut-matrix-lapack) asum
  (for-all-implementations
    (ensure (= (asum (ones 2 2))
           4d0))
    (ensure (= (asum (ones 2 2 :element-type 'single-float))
           4.0))
    (ensure (= (asum (ones 2 2 :element-type '(complex single-float)))
           #C(4.0 0.0)))
    (ensure (= (asum (ones 2 2 :element-type '(complex double-float)))
           #C(4d0 0d0)))))

(addtest (lisp-matrix-ut-matrix-lapack) iamax
  (for-all-implementations
    (ensure (= (iamax (make-matrix 2 2 :initial-contents '((1d0 2d0)
                                                       (1d0 1d0))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type 'single-float
                                   :initial-contents '((1.0 2.0)
                                                       (1.0 1.0))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type '(complex single-float)
                                   :initial-contents '((#C(1.0 0.0) #C(2.0 0.0))
                                                       (#C(1.0 0.0) #C(1.0 0.0)))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type '(complex double-float)
                                   :initial-contents '((#C(1d0 0d0) #C(2d0 0d0))
                                                       (#C(1d0 0d0) #C(1d0 0d0)))))
           2))
    (ensure (= (iamax (ones 1 1))
           0))
    (ensure (= (iamax (ones 1 1 :element-type 'single-float))
           0))
    (ensure (= (iamax (ones 1 1 :element-type '(complex single-float)))
           0))
    (ensure (= (iamax (ones 1 1 :element-type '(complex double-float)))
           0))))


;;;; GEMM tests


;; Foreign array problems...  Currently error, not failure!
(addtest (lisp-matrix-ut-matrix-gemm)
  sticky-matmult-cases
  (let ((m2-fa (make-matrix
		2 5
		:implementation :foreign-array 
		:element-type 'double-float
		:initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
				      ( 6d0 7d0 8d0 9d0 10d0))))
	(m3-fa (make-matrix
		2 2
		:implementation :foreign-array 
		:element-type 'double-float
		:initial-contents #2A(( 1d0 2d0 )
				      ( 6d0 7d0 )))))

    (m* m3-fa m3-fa)
    (m* m2-fa (transpose-matrix m2-fa))
    (m* m3-fa (transpose-matrix m3-fa))))

;;; Working, but needs to be extended for testing!
(addtest (lisp-matrix-ut-matrix-gemm)
  working-matmult-cases
  (let ((m3-la (make-matrix
		2 2
		:implementation :lisp-array 
		:element-type 'double-float
		:initial-contents #2A(( 1d0 2d0 )
				      ( 6d0 7d0 )))))
    (m* m3-la m3-la)
    (m* m3-la (transpose-matrix m3-la))))


(defun check-m* (a b)
  (let ((result (make-matrix 2 2 :initial-contents
                             '((19d0 22d0)
                               (43d0 50d0)))))
    (ensure (m= result (m* a b)))))


(def-m*-test m*-basic-test
  (make-matrix 2 2 :initial-contents
	       '((1d0 2d0)
		 (3d0 4d0)))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-a
  (transpose-matrix
   (make-matrix 2 2 :initial-contents
		'((1d0 3d0)
		  (2d0 4d0))))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-b
  (make-matrix 2 2 :initial-contents
	       '((1d0 2d0)
		 (3d0 4d0)))
  (transpose-matrix
   (make-matrix 2 2 :initial-contents
                '((5d0 7d0)
                  (6d0 8d0)))))

(def-m*-test m*-double-transpose-a
    (transpose-matrix
     (transpose-matrix
      (make-matrix 2 2 :initial-contents
                   '((1d0 2d0)
                     (3d0 4d0)))))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-a-b
    (transpose-matrix
     (make-matrix 2 2 :initial-contents
                  '((1d0 3d0)
                    (2d0 4d0))))
  (transpose-matrix
   (make-matrix 2 2 :initial-contents
                '((5d0 7d0)
                  (6d0 8d0)))))

(def-m*-test m*-window-a-nocopy
    (window
     (make-matrix 3 3 :initial-contents
                  '((1d0 2d0 0d0)
                    (3d0 4d0 0d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-window-a-copy
    (window
     (make-matrix 3 3 :initial-contents
                  '((0d0 1d0 2d0)
                    (0d0 3d0 4d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2 :col-offset 1)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-window-b-nocopy
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (window
   (make-matrix 2 3 :initial-contents
                '((5d0 6d0 0d0)
                  (7d0 8d0 0d0)))
   :ncols 2))

(def-m*-test m*-window-b-copy
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (window
   (make-matrix 3 3 :initial-contents
                '((0d0 0d0 0d0)
                  (5d0 6d0 0d0)
                  (7d0 8d0 0d0)))
   :ncols 2 :nrows 2 :row-offset 1))

(def-m*-test m*-stride-a-nocopy
    (strides
     (make-matrix 3 3 :initial-contents
                  '((1d0 2d0 0d0)
                    (3d0 4d0 0d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-stride-a-copy
    (strides
     (make-matrix 4 3 :initial-contents
                  '((1d0 0d0 2d0)
                    (0d0 0d0 0d0)
                    (3d0 0d0 4d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2 :row-stride 2 :col-stride 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(addtest (lisp-matrix-ut-matrix-gemm)
  gemm-window-c-copy
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 3 3))
           (windowed-c (window c :nrows 2 :ncols 2)))
      (ensure (eq windowed-c
              (gemm 1d0
                    (make-matrix 2 2 :initial-contents
                                 '((1d0 2d0)
                                   (3d0 4d0)))
                    (make-matrix 2 2 :initial-contents
                                 '((5d0 6d0)
                                   (7d0 8d0)))
                    0d0
                    windowed-c)))
      (ensure (m= windowed-c result))
      (ensure (m= windowed-c (window c :nrows 2 :ncols 2)))
      (ensure (m= (window c :nrows 1 :row-offset 2)
              (zeros 1 3)))
      (ensure (m= (window c :ncols 1 :col-offset 2)
              (zeros 3 1))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  gemm-window-c-copy-copyback
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 4 4))
           (windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
                                                   :col-offset 2)))
      (ensure (eq windowed-c
		  (gemm 1d0
			(make-matrix 2 2 :initial-contents
				     '((1d0 2d0)
				       (3d0 4d0)))
			(make-matrix 2 2 :initial-contents
				     '((5d0 6d0)
				       (7d0 8d0)))
			0d0
			windowed-c)))
      (ensure (m= windowed-c result))
      (ensure (m= windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
				     :col-offset 2)))
      (ensure (m= (window c :nrows 2) (zeros 2 4)))
      (ensure (m= (window c :ncols 2) (zeros 4 2))))))


(addtest (lisp-matrix-ut-matrix-gemm)
  m*-double
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type 'double-float
                            :initial-contents '((1d0 2d0 0d0)
                                                (3d0 4d0 0d0)
                                                (0d0 0d0 0d0)))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type 'double-float
                           :initial-contents '((5d0 6d0)
                                               (7d0 8d0))))
      (make-matrix 2 2 :element-type 'double-float
                       :initial-contents '((19d0 22d0)
                                           (43d0 50d0)))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-single
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type 'single-float
                            :initial-contents '((1.0 2.0 0.0)
                                                (3.0 4.0 0.0)
                                                (0.0 0.0 0.0)))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type 'single-float
                           :initial-contents '((5.0 6.0)
                                               (7.0 8.0))))
      (make-matrix 2 2 :element-type 'single-float
                       :initial-contents '((19.0 22.0)
                                           (43.0 50.0)))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-complex-single
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type '(complex single-float)
                            :initial-contents
                            '((#C(1.0 0.0) #C(2.0 0.0) #C(0.0 0.0))
                              (#C(3.0 0.0) #C(4.0 0.0) #C(0.0 0.0))
                              (#C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0))))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type '(complex single-float)
                           :initial-contents
                           '((#C(5.0 0.0) #C(6.0 0.0))
                             (#C(7.0 0.0) #C(8.0 0.0)))))
      (make-matrix 2 2 :element-type '(complex single-float)
                       :initial-contents '((#C(19.0 0.0) #C(22.0 0.0))
                                           (#C(43.0 0.0) #C(50.0 0.0))))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-complex-double
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type '(complex double-float)
                            :initial-contents
                            '((#C(1d0 0d0) #C(2d0 0d0) #C(0d0 0d0))
                              (#C(3d0 0d0) #C(4d0 0d0) #C(0d0 0d0))
                              (#C(0d0 0d0) #C(0d0 0d0) #C(0d0 0d0))))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type '(complex double-float)
                           :initial-contents
                           '((#C(5d0 0d0) #C(6d0 0d0))
                             (#C(7d0 0d0) #C(8d0 0d0)))))
      (make-matrix 2 2 :element-type '(complex double-float)
                       :initial-contents '((#C(19d0 0d0) #C(22d0 0d0))
                                           (#C(43d0 0d0) #C(50d0
  0d0))))))))

(addtest (lisp-matrix-ut-matrix-gemm)
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




(addtest (lisp-matrix-ut-matrix-lapack)
  m+
  (for-all-implementations
    (let* ((a (ones 2 2))
           (b (scal 2d0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2d0 (ones 2 2))))
      (ensure (m= (m+ a b) (scal 3d0 (ones 2 2)))))
    (let* ((*default-element-type* 'single-float)
           (a (ones 2 2))
           (b (scal 2.0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2.0 (ones 2 2))))
      (ensure (m= (m+ a b) (scal 3.0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float))
           (a (ones 2 2))
           (b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= (m+ a b) (scal #C(3.0 2.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float))
           (a (ones 2 2))
           (b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= (m+ a b) (scal #C(3d0 2d0) (ones 2 2)))))))

(addtest (lisp-matrix-ut-matrix-lapack)
  m-
  (for-all-implementations
    (let* ((a (ones 2 2))
           (b (scal 2d0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2d0 (ones 2 2))))
      (ensure (m= (m- a b) (scal -1d0 (ones 2 2)))))
    (let* ((*default-element-type* 'single-float)
           (a (ones 2 2))
           (b (scal 2.0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2.0 (ones 2 2))))
      (ensure (m= (m- a b) (scal -1.0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float))
           (a (ones 2 2))
           (b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2.0 2.0) (ones 2 2)))) 
      (ensure (m= (m- a b) (scal #C(-1.0 -2.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float))
           (a (ones 2 2))
           (b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= (m- a b) (scal #C(-1d0 -2d0) (ones 2 2)))))))



;;;;;;;;; NOT IN FIX PROCESS FIXME.
#|

(addtest (lisp-matrix-ut) copy
  (for-all-implementations
    (labels ((test-copy-m= (a b)
               (and (not (eq a b))
                    (m= a b)))
             (test-copy (a)
               (let ((b (copy a))
                     (c (make-matrix (nrows a) (ncols a)
                                     :element-type (element-type a)
                                     :implementation (implementation a))))
                 (finishes (copy! a c))
                 (ensure (test-copy-m= a b))
                 (ensure (test-copy-m= b c))
                 (ensure (test-copy-m= a c)))))
      (for-all ((n (gen-integer :min 0 :max 10) #+clisp (> n 0))
                (m (gen-integer :min 0 :max 10) #+clisp (> m 0))
                (n2 (gen-integer :min 0 :max 10)
                    (and (<= n2 n) #+clisp (> n2 0)))
                (m2 (gen-integer :min 0 :max 10)
                    (and (<= m2 m) #+clisp (> m2 0)))
                (row-offset (gen-integer :min 0 :max 10)
                            (<= row-offset (- n n2)))
                (col-offset (gen-integer :min 0 :max 10)
                            (<= col-offset (- m m2))))
        (test-copy (rand n m))
        (test-copy (transpose-matrix (rand n m)))
        (test-copy (window (rand n m)
                           :nrows n2 :ncols m2
                           :row-offset row-offset
                           :col-offset col-offset))))))

|#
