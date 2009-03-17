;;; Demos for Lisp Matrix (encoded within progn's)
;;;
;;; = instantiating matrices and vectors
;;; = inversion using BLAS/LAPACK
;;; 
;;;

(in-package :lisp-matrix-user)

(progn ;; data object instantiation

  (defparameter *m01*
    (make-matrix
     6 5
     :initial-contents '((11d0 12d0 13d0 14d0 15d0)
			 (21d0 22d0 23d0 24d0 25d0)
			 (31d0 32d0 33d0 34d0 35d0)
			 (41d0 42d0 43d0 44d0 45d0)
			 (51d0 52d0 53d0 54d0 55d0)
			 (61d0 62d0 63d0 64d0 65d0)))
    "6x5 matrix with entries representing row+1,col+1 values, for
     test purposes.")

  (documentation  '*m01* 'variable)

  (defparameter *m1-ex*  (make-matrix 2 5
			   :implementation :lisp-array  ;; :foreign-array
			   :element-type 'double-float)
    "quick variable initialized to zeros")
    
  (defparameter *m2-la-int*
    (make-matrix 2 5
		 :implementation :lisp-array  ;; :foreign-array
		 :element-type 'integer ; 'double-float
		 ;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
		 :initial-contents #2A((1 2 3 4 5)
				       (6 7 8 9 10)))
    "placeholder 2")

  ;; Currently we can make a foriegn matrix of doubles, but not a
  ;; foriegn matrix of integers.
  (defparameter *m2-fa*
    (make-matrix
     2 5
     :implementation :foreign-array 
     :element-type 'double-float
     :initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
			   ( 6d0 7d0 8d0 9d0 10d0)))
    "placeholder 2")

  (defparameter *m2-la*
    (make-matrix
     2 5
     :implementation :lisp-array 
     :element-type 'double-float
     :initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
			   ( 6d0 7d0 8d0 9d0 10d0)))
    "placeholder 2")


  (defparameter *m3-fa*
    (make-matrix
     2 2
     :implementation :foreign-array 
     :element-type 'double-float
     :initial-contents #2A(( 1d0 2d0 )
			   ( 6d0 7d0 )))
    "placeholder 2")

  (defparameter *m3-la*
    (make-matrix
     2 2
     :implementation :lisp-array 
     :element-type 'double-float
     :initial-contents #2A(( 1d0 2d0 )
			   ( 6d0 7d0 )))
    "placeholder 2")

    
  (defparameter *m01b*
    (strides *m01* :nrows 2 :ncols 3
	     :row-stride 2
	     :row-offset 1 :col-offset 1))
  
  (defparameter *m01c* 
    (window *m01*
	    :nrows 2 :ncols 3
	    :row-offset 2 :col-offset 1))
					; EVAL BELOW TO SETUP DATA


  ;; data for lls estimation
  (defparameter *xv*
    (make-vector
     8
     :type :row ;; default, not usually needed!
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0))))

  ;; col vector
  (defparameter *xv2*
    (make-vector
     8
     :type :column
     :initial-contents '((1d0)
			 (3d0)
			 (2d0)
			 (4d0)
			 (3d0)
			 (5d0)
			 (4d0)
			 (6d0))))

  (v= *xv* *xv2*) ; => T
  (m= *xv* *xv2*) ; => nil

  (defparameter *xv+1*
    (make-matrix
     8 2
     :initial-contents '((1d0 1d0)
			 (1d0 3d0)
			 (1d0 2d0)
			 (1d0 4d0)
			 (1d0 3d0)
			 (1d0 5d0)
			 (1d0 4d0)
			 (1d0 6d0))))

  (defparameter *xv+1a*
    (make-matrix
     8 2
     :initial-contents #2A((1d0 1d0)
			   (1d0 3d0)
			   (1d0 2d0)
			   (1d0 4d0)
			   (1d0 3d0)
			   (1d0 5d0)
			   (1d0 4d0)
			   (1d0 6d0))))

  (defparameter *xv+1b*
    (bind2
     (ones 8 1)
     (make-matrix
      8 1
      :initial-contents '((1d0)
			  (3d0)
			  (2d0)
			  (4d0)
			  (3d0)
			  (5d0)
			  (4d0)
			  (6d0)))
     :by :column))

  (m= *xv+1a* *xv+1b*) ; => T

  (defparameter *xm*
    (make-matrix
     2 8
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0)
			 (1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  (defparameter *y2*
    (make-vector
     8
     :type :column
     :initial-contents '((1d0)
			 (2d0)
			 (3d0)
			 (4d0)
			 (5d0)
			 (6d0)
			 (7d0)
			 (8d0))))
  (transpose *y2*)




  (format nil "Data set up"))

#+nil
(progn 
  ;; Tests for square matrices...
  (trap2mat (rand 3 3))

  (trap2mat (make-matrix 3 3
			 :initial-contents #2A((1d0 2d0 3d0)
					       (4d0 5d0 6d0)
					       (7d0 8d0 9d0))))
  (trap2mat (make-matrix 3 3
			 :initial-contents #2A((1d0 2d0 3d0)
					       (4d0 5d0 6d0)
					       (7d0 8d0 9d0)))
	    :type :lower)
  (trap2mat (make-matrix 3 3
			 :initial-contents #2A((1d0 2d0 3d0)
					       (4d0 5d0 6d0)
					       (7d0 8d0 9d0)))
	    :type :upper)

  ;; need to write unit tests for square and rect matrices.
  )


#+nil
(progn
  ;; factorization and inversion via LAPACK

  ;; LU
  (let ((test-eye (eye 7 7)))
    (m* test-eye (minv-lu test-eye)))

  ;; Cholesky
  (let ((myrand (rand 4 4)))
    (princ myrand)
    (princ (matrix-like-symmetric-p (m* (transpose myrand) myrand)))
    (princ (m*  (m* (transpose myrand) myrand)
		(minv-cholesky  (m* (transpose myrand) myrand))))))


(progn  
  ;; Using xGEQRF routines for supporting linear regression.

  ;; Question: Need to incorporate the xGEQRF routines, to support
  ;; linear regression work?

  ;; LAPACK suggests to use the xGELSY driver (GE general matrix, LS
  ;; least squares, need to lookup Y intent (used to be an X alg, see
  ;; release notes).

  (let ((a (rand 10 5)))
    (geqrf a))

  )

