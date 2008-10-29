;;; Precursor systems

;;  (asdf:oos 'asdf:compile-op 'ffa :force t)

;;  (asdf:oos 'asdf:compile-op 'org.middleangle.foreign-numeric-vector :force t)
;;  (asdf:oos 'asdf:compile-op 'org.middleangle.cl-blapack :force t)

;;; The maing thing...
;; (asdf:oos 'asdf:compile-op 'lisp-matrix :force t)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

(progn ;; THESE WORK!
  
  ;; make some matrices
  (defvar m1 nil
    "placeholder 1")
  (setf m1 (make-matrix 2 5
			:implementation :lisp-array  ;; :foreign-array
			:element-type 'double-float))
  

  ;; works, as it should.
  (mref m1 1 3)
  (setf (mref m1 1 3) 1.2d0)
  m1


  ;; increase complexity

  (defvar m2 nil
    "placeholder 2")
  (setf m2 (make-matrix 2 5
			:implementation :lisp-array  ;; :foreign-array
			:element-type 'integer ; 'double-float
			;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10))))



  ;; Currently we can make a foriegn matrix of doubles, but not a
  ;; foriegn matrix of integers.
  (defvar m2b nil
    "placeholder 2")
  (setf m2b (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'double-float
			:initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
					      ( 6d0 7d0 8d0 9d0 10d0))))


  (mref m2 0 2) ;; -> 3
  m2
  (transpose m2)

  (defvar m3 nil
    "placeholder 3")

  (setf m3 (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
  (row m3 2)
  (col m3 1)


  (mref m3 0 1)
  (mref (transpose m3) 1 0)

  (mref m3 2 2)
  (mref (transpose m3) 2 2)

  m3
  (transpose m3)

  (defvar m4 nil
    "yet another placeholder.")
  (setf m4 (strides m3 :nrows 2 :row-stride 2))
  m4
  (m= (row m4 0)
      (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))
  (m= (row m4 1)
      (make-matrix 1 5 :initial-contents '((11d0 12d0 13d0 14d0 15d0))))
  ;; note the redoing for the columns -- different!
  (m= (col m4 0)
      (make-matrix 2 1 :initial-contents '((1d0) (11d0))))
  (m= (col m4 1)
      (make-matrix 2 1 :initial-contents '((2d0) (12d0))))

  (v= (row m4 0) (col (transpose m4) 0))
  (v= (col m4 0) (row (transpose m4) 0))

  m4
  (row m4 0)
  (col m4 4)


  (let* ((*default-element-type* '(complex double-float))
	 (m1 (axpy #C(1.0d0 0.0d0)
		   (ones 2 2)
		   (scal #C(1.5d0 0.0d0)
			 (ones 2 2))))
	 (m2 (scal #C(2.5d0 0.0d0) (ones 2 2)))
	 (m3 (axpy #C(-1.0d0 0.0d0)
		   (ones 2 2)
		   (scal #C(1.5d0 0.0d0) (ones 2 2))))
	 (m4 (scal #C(0.5d0 0.0d0) (ones 2 2))))
    (format t "~A ~A ~%"
	    (m= m1 m2)
	    (m= m3 m4)))

  (m+ (row m3 1) (row m3 2))
  (m- (row m3 1) (row m3 2))

  )


(progn ;; FIX ALL THE ERRORS

  ;; FIXME: need to get the foriegn-friendly arrays package involved. 
  (defvar m2a nil
    "placeholder 2")
  (setf m2a (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10))))

  ;; FIXME -- bad error!!
  ;; This index isn't correct, and it doesn't barf correctly. 
  (mref m1 2 3)
  (setf (mref m1 2 3) 1.2d0)
  m1

  ;; FIXME: the following has no applicable method!
  (m* m2 (transpose m2))

  m4
  (transpose m4)
  ;; given the above...
  ;; FIXME: Big Barf!
  (v= (row m4 1)
      (col (transpose m4) 1) ) ;; fails ???

  ;; and the same problem.
  m3 
  (transpose m3)
  (v= (col m3 1) (row (transpose m3) 1))
  (v= (row m3 1) (col (transpose m3) 1))


  )


;;; EXAMPLES TO DEMONSTRATE


;;; consider the following matrix:
;;; n1= 11 12 13
;;;     21 22 23
(defparameter *n1*
  (make-matrix 2 3
	       :implementation :lisp-array
	       :element-type 'double-float
	       :initial-contents #2A ((11d0 12d0 13d0)
				      (21d0 22d0 23d0))))
*n1*
;;; then storage in row-major orientation would be a sequence
;;;     11 12 13 21 22 23
;;; while in column-major orientation it would be
;;;     11 21 12 22 13 23 
;;; At this point, consider the following.  Suppose we have a matview
;;; with dims 1x3, row/col offset 1,0:
;;; n2= 21 22 23
(defparameter *n2*
  (window *n1*
	  :nrows 1 :ncols 3
	  :row-offset 1 :col-offset 0))
*n2*
;;; or alternatively dims 2x2, row/col offset 0,1:
;;; n3= 12 13
;;;     22 23
(defparameter *n3*
  (window *n1*
	  :nrows 2 :ncols 2
	  :row-offset 0 :col-offset 1))
*n3*
;;;
;;; for the first, we see that, by orientation, we have the following:
;;;     .. .. .. 21 22 23   (row-major)
;;;     .. 21 .. 22 .. 23   (column-major)
;;; 
;;; so we see that for
;;; row-major:    index=3 (ncols), stride=1
;;; column-major: index=1 (ncols), stride=2 (nrows)
;;; 
;;; for the second, by orientation, we have:
;;;     .. 12 13 .. 22 23  (row-major)
;;;     .. 12 22 .. 13 23  (column-major)
;;; 
;;; so we see that for
;;; row-major:    index=1 (ncols), stride=2 (ncols)
;;; column-major: index=1,(nrows), stride=3 (nrows)
;;; 
;;; Consider a more complex matrix:
;;; 
;;; o1= 11 12 13 14 15
;;;     21 22 23 24 25
;;;     31 32 33 34 35
;;;     41 42 43 44 45
(defparameter *o1*
  (make-matrix 4 5
	       :implementation :lisp-array
	       :element-type 'double-float
	       :initial-contents #2A ((11d0 12d0 13d0 14d0 15d0)
				      (21d0 22d0 23d0 24d0 25d0)
				      (31d0 32d0 33d0 34d0 35d0)
				      (41d0 42d0 43d0 44d0 45d0))))
*o1*
;;; row-major:
;;;    o1= 11 12 13 14 15 21 22 23 24 25 31 32 33 34 35 41 42 43 44 45
;;; col-major: 
;;;    o1= 11 21 31 41 12 22 32 42 13 23 33 43 14 24 34 44 15 25 35 45
;;;
;;;
;;; Then a matview, dims 3, offset 2,1 :
;;;
;;; o2= 32 33 34
;;;     42 43 44
(defparameter *o2*
  (window *o1*
	  :nrows 2 :ncols 3
	  :row-offset 2 :col-offset 1))
*o2*
;;;
;;; and a strided matview, indexed, could be (offset 2,3; row-stride 2)
;;;
;;; o3= 23 24 25
;;;     43 44 45
(defparameter *o3*
  (strides *o1*
	   :nrows 2 :ncols 3
	   :row-offset 1 :col-offset 2
	   :row-stride 2 :col-stride 1))
*o3*
;;; and for where this sits in the original matrix...
;;;
;;; and now to pull out the rows and columns via slicing on a strided
;;; matrix, we have the following approaches, for the zero-th column:
;;;     23
;;;     43
(slice *o3* :offset 0 :stride 1 :nelts (nrows *o3*) :type :column)
(parent *o3*)
;;; and for the 2nd column (3rd, since we are zero counting).
;;;     25
;;;     45
(slice *o3* :offset 4 :stride 1 :nelts (nrows *o3*) :type :column)
;;; and for the 1st row (2nd, again zero-counting):
;;;     43 44 45
(slice *o3* :offset 1 :stride 2 :nelts (ncols *o3*) :type :row)
;;; 
(orientation *o3*)

;; convert between foriegn-array and lisp-array.

;; operate ()

;; do some blas/lapack

;; output

;; Windowing -- simple, works!
(m= (col *c* 0)
    (make-matrix 3 1 :initial-contents '((16d0) (21d0) (26d0))))
(m= (col *c* 1) 
    (make-matrix 3 1 :initial-contents '((17d0) (22d0) (27d0))))
(m= (col *c* 2)
    (make-matrix 3 1 :initial-contents '((18d0) (23d0) (28d0))))
(m= (col *c* 3)
    (make-matrix 3 1 :initial-contents '((19d0) (24d0) (29d0))))
(m= (col *c* 4)
    (make-matrix 3 1 :initial-contents '((20d0) (25d0) (30d0))))

(m= (col *d* 0)
    (make-matrix 3 1 :initial-contents '((18d0) (23d0) (28d0))))
(m= (col *d* 1) 
    (make-matrix 3 1 :initial-contents '((19d0) (24d0) (29d0))))

;; do we want this as part of the API? Currently fails.
;; (m= (col *c* 4)
;;     (col *c* 4)
;;     (make-matrix 3 1 :initial-contents '((20d0) (25d0) (30d0))))
