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

(deftestsuite lisp-matrix-ut-matrix  (lisp-matrix-ut) ())

;;; SUPPORT FUNCTIONS

;;; TESTS: MATRICES toplevel/general

;;; FIXME
(addtest (lisp-matrix-ut-matrix)
  matrix-foreign-array-integer-values
  ;; FIXME: integer-valued foreign arrays. (bug: matrix-2)
  (ensure  (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10)))))

(addtest (lisp-matrix-ut-matrix)
  wrong-data-initially
  (ensure-error  ;; because data is integer, not double-float!
    (let ((m1  (make-matrix 2 5
		      :implementation :lisp-array 
		      :element-type 'double-float
		      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					  (6d0 7 8 9 10)))))
      m1)))

(addtest (lisp-matrix-ut-matrix)
  right-data-initially
  (let ((n 2)
	(m 5)
	(m1 (make-matrix 2 5
			 :implementation :lisp-array 
			 :element-type 'double-float
			 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					     (6d0 7d0 8d0 9d0 10d0)))))
    (ensure (= (nrows m1) n))
    (ensure (= (ncols m1) m))
    (ensure (= (nelts m1) (* n m)))
    (ensure (= (matrix-dimension m1 0) n))
    (ensure (= (matrix-dimension m1 1) m))
    (ensure-error (matrix-dimension m1 2))
    (ensure-error (matrix-dimension m1 -1))
    (ensure (equal (matrix-dimensions m1)
		   (list n m)))))
   

;; combinations...
(addtest (lisp-matrix-ut-matrix)
  data-initialize
  (ensure-error  
    ;; because data is integer, not double-float!
    (let ((m1  (make-matrix 2 5
		      :implementation :lisp-array 
		      :element-type 'double-float
		      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					  (6d0 7 8 9 10)))))
      m1))
  (ensure
   ;; correct initial data
   (let ((m1 (make-matrix 2 5
			  :implementation :lisp-array 
			  :element-type 'double-float
			  :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					      (6d0 7d0 8d0 9d0 10d0)))))
     m1)))


;; combination + progn
(addtest (lisp-matrix-ut-matrix)
  data-initialize-2
  (progn
    (ensure-error  
      ;; because data is integer, not double-float!
      (let ((m1  (make-matrix 2 5
			      :implementation :lisp-array 
			      :element-type 'double-float
			      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						  (6d0 7 8 9 10)))))
	m1))
    (ensure
     ;; correct data input
     (let ((m1 (make-matrix 2 5
			    :implementation :lisp-array 
			    :element-type 'double-float
			    :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						(6d0 7d0 8d0 9d0 10d0)))))
       m1))))



(addtest (lisp-matrix-ut-matrix)
  one-random-test-2
  (test-matrix-size (make-matrix 2 5
				 :implementation :lisp-array 
				 :element-type 'double-float
				 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						     (6d0 7d0 8d0 9d0 10d0)))
		    2 5))

;; FIXME: see comments after the next two tests, for additional issues
;; slipping through the tests (enforced symmetry).

(addtest (lisp-matrix-ut-matrix)
  indexing-getting-matrix
  (let (( m1 (make-matrix 2 5
			  :implementation :lisp-array 
			  :element-type 'double-float
			  :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					      (6d0 7d0 8d0 9d0 10d0)))))
    (ensure (mref m1 1 1))
    (ensure (mref m1 0 0))
    (ensure (mref m1 1 4))
    (ensure-error (mref m1 2 5)) ;; both above
    (ensure-error (mref m1 1 5))
    (ensure-error (mref m1 2 4))))


(addtest (lisp-matrix-ut-matrix)
  indexing-setting-matrix
  (let (( m1 (make-matrix 2 5
			  :implementation :lisp-array 
			  :element-type 'double-float
			  :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					      (6d0 7d0 8d0 9d0 10d0)))))
    (ensure (setf (mref m1 1 1) -1d0))
    (ensure (setf (mref m1 0 0) -1d0))
    (ensure (setf (mref m1 1 4) -1d0))
    (ensure-error (setf  (mref m1 2 5) -1d0 ) ) ;; both above
    (ensure-error (setf  (mref m1 1 5) -1d0 ) )
    (ensure-error (setf  (mref m1 2 4) -1d0 ) )))

;;; FIXME: The above 2 need to do a bit more verification when set
;;; that we get the right result.  In particular, setting/getting
;;; should mirror when errors occur.


(addtest (lisp-matrix-ut-matrix)
  make-matrix-double-zero-size
  #-clisp (for-all-implementations
	    (ensure (make-matrix 0 0))
	    (ensure (make-matrix 0 1))
	    (ensure (make-matrix 1 0)))
  #+clisp (for-implementations (:lisp-array) ;; foreign zero-size arrays fail in CLISP?
	    (finishes (make-matrix 0 0))
	    (finishes (make-matrix 0 1))
	    (finishes (make-matrix 1 0))))

(addtest (lisp-matrix-ut-matrix)
  transposed-p
  (for-all-implementations
    (let ((m (make-matrix 2 2)))
      (ensure (null (transposed-p m)))
      (ensure (transposed-p (transpose-matrix m)))
      (ensure (transposed-p (window (transpose-matrix m))))
      ;; the last one was removed because now the transpose of a
      ;; transpose returns the original matrix
      #+(or)
      (ensure (transposed-p (transpose-matrix (transpose-matrix m)))))))

(addtest (lisp-matrix-ut-matrix)
  m=
  (for-all-implementations
    (ensure (not (m= (make-matrix 1 2)
		     (make-matrix 1 1))))
    (ensure (not (m= (make-matrix 2 1)
		     (make-matrix 1 1))))
    (ensure (not (m= (make-matrix 1 1 :initial-element 1d0)
		     (make-matrix 1 1 :initial-element 0d0))))))


(addtest (lisp-matrix-ut-matrix)
  zero-offset-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (ensure (zero-offset-p m))
      (ensure (zero-offset-p (transpose-matrix m)))
      (ensure (zero-offset-p (transpose-matrix (transpose-matrix m))))
      (ensure (zero-offset-p (window m :nrows 1)))
      (ensure (zero-offset-p (strides m :ncols 1)))
      (ensure (not (zero-offset-p (window m :row-offset 1 :nrows 1))))
      (ensure (not (zero-offset-p (window m :col-offset 1 :ncols 1))))
      (ensure (not (zero-offset-p (strides m :row-offset 1 :nrows 1))))
      (ensure (not (zero-offset-p (strides m :col-offset 1 :ncols 1))))
      (ensure (not (zero-offset-p (window (strides m :col-offset 1 :ncols 1)))))
      (ensure (zero-offset-p (strides m :row-stride 2 :nrows 2))))))

(addtest (lisp-matrix-ut) unit-strides-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (ensure (unit-strides-p m))
      (ensure (unit-strides-p (transpose-matrix m)))
      (ensure (unit-strides-p (transpose-matrix (transpose-matrix m))))
      (ensure (unit-strides-p (window m :nrows 1)))
      (ensure (unit-strides-p (strides m :ncols 1)))
      (ensure (unit-strides-p (window m :row-offset 1 :nrows 1)))
      (ensure (unit-strides-p (window m :col-offset 1 :ncols 1)))
      (ensure (unit-strides-p (strides m :row-offset 1 :nrows 1)))
      (ensure (unit-strides-p (strides m :col-offset 1 :ncols 1)))
      (ensure (not (unit-strides-p (strides m :row-stride 2 :nrows 2))))
      (ensure (not (unit-strides-p (transpose-matrix (strides m :row-stride 2 :nrows 2)))))
      (ensure (not (unit-strides-p (window (strides m :row-stride 2 :nrows 2)))))
      (ensure (not (unit-strides-p (strides (strides m :row-stride 2 :nrows 2))))))))



;;; Matrix creation

(addtest (lisp-matrix-ut-matrix)
  ones
  (for-all-implementations
    (ensure (m= (ones 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((1.0 1.0)
                                             (1.0 1.0)))))
    (ensure (m= (ones 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((1d0 1d0)
                                             (1d0 1d0)))))
    (ensure (m= (ones 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents '((#C(1.0 0.0) #C(1.0 0.0))
                                             (#C(1.0 0.0) #C(1.0 0.0))))))
    (ensure (m= (ones 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents
                         '((#C(1d0 0d0) #C(1d0 0d0))
                           (#C(1d0 0d0) #C(1d0 0d0))))))))

(addtest (lisp-matrix-ut-matrix) zeros
  (for-all-implementations
    (ensure (m= (zeros 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((0.0 0.0)
                                             (0.0 0.0)))))
    (ensure (m= (zeros 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((0d0 0d0)
                                             (0d0 0d0)))))
    (ensure (m= (zeros 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents
                         '((#C(0.0 0.0) #C(0.0 0.0))
                           (#C(0.0 0.0) #C(0.0 0.0))))))
    (ensure (m= (zeros 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents
                         '((#C(0d0 0d0) #C(0d0 0d0))
                           (#C(0d0 0d0) #C(0d0 0d0))))))))

(addtest (lisp-matrix-ut-matrix) eye
  (for-all-implementations
    (ensure (m= (eye 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((1.0 0.0)
                                             (0.0 1.0)))))
    (ensure (m= (eye 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((1d0 0d0)
                                             (0d0 1d0)))))
    (ensure (m= (eye 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents
                         '((#C(1.0 0.0) #C(0.0 0.0))
                           (#C(0.0 0.0) #C(1.0 0.0))))))
    (ensure (m= (eye 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents '((#C(1d0 0d0) #C(0d0 0d0))
                                             (#C(0d0 0d0) #C(1d0 0d0))))))))

(addtest (lisp-matrix-ut-matrix)
  rand
  (for-all-implementations
    (let* ((state1 (make-random-state))
           (state2 (make-random-state state1)))
      (ensure (m= (rand 2 3 :state state1)
              (rand 2 3 :state state2)))
      (ensure (not (m= (rand 2 3 :state state1)
                   (rand 2 3 :state state1)))))))


(addtest (lisp-matrix-ut-matrix)
  bind2-dims-conditions
  (for-all-implementations
    (let ((m1 (zeros 2 3))
	  (m2 (zeros 2 2))
	  (m3 (zeros 3 2)))
      ;; one order of the conditions based on dimension ineq
      (ensure (m= (bind2 m1 m2 :by :column)
		  (zeros 2 5)))
      (ensure (m= (bind2 m2 m3 :by :row)
		  (zeros 5 2)))
      ;; other order of the conditions based on dimension ineq
      (ensure (m= (bind2 m2 m1 :by :column)
		  (zeros 2 5)))
      (ensure (m= (bind2 m3 m2 :by :row)
		  (zeros 5 2))))))




(addtest (lisp-matrix-ut-matrix)
  r-apply-columns
  (for-all-implementations
    (let ((m1 (ones 2 3))
	  (m2 (ones 3 2)))
      ;; one order of the conditions based on dimension ineq
      (let ((col-list (list-of-columns m1)))
	(dotimes (i (length col-list))
	  (ensure (v= (nth i col-list)
		      (ones 2 1))))))))

