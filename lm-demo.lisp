(progn ;; SETUP DATA, these work

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

  ;; Tests for square matrices...
  (defparameter *test-trap2mat-1*
    (trap2mat (rand 3 3)))

  (defparameter *test-trap2mat-2*
    (trap2mat (make-matrix 3 3
			   :initial-contents #2A((1d0 2d0 3d0)
						 (4d0 5d0 6d0)
						 (7d0 8d0 9d0)))))

  (defparameter *test-trap2mat-3*
    (trap2mat (make-matrix 3 3
			   :initial-contents #2A((1d0 2d0 3d0)
						 (4d0 5d0 6d0)
						 (7d0 8d0 9d0)))
	      :type :lower))

  (defparameter *test-trap2mat-4*
    (trap2mat (make-matrix 3 3
			   :initial-contents #2A((1d0 2d0 3d0)
						 (4d0 5d0 6d0)
						 (7d0 8d0 9d0)))
	      :type :upper))




  ;; need to write unit tests for square and rect matrices.

  (format nil "Data set up"))



(progn  
  ;; Using xGEQRF routines for supporting linear regression.

  ;; Some issues exist in the LAPACK vs. LINPACK variants, hence R
  ;; uses LINPACK primarily, rather than LAPACK.  See comments in R
  ;; source for issues.  

  ;; Question: Need to incorporate the xGEQRF routines, to support
  ;; linear regression work?

  ;; LAPACK suggests to use the xGELSY driver (GE general matrix, LS
  ;; least squares, need to lookup Y intent (used to be an X alg, see
  ;; release notes).

  ;; Goal is to start from X, Y and then realize that if
  ;; Y = X \beta, then,   i.e. 8x1 = 8xp px1  + 8x1
  ;;      XtX \hat\beta = Xt Y
  ;; so that we can solve the equation  W \beta = Z   where W and Z
  ;; are known, to estimate \beta.

  ;; the above is known to be numerically instable -- some processing
  ;; of X is preferred and should be done prior.  And most of the
  ;; transformation-based work does precisely that.

  ;; recall:  Var[Y] = E[(Y - E[Y])(Y-E[Y])t]
  ;;   = E[Y Yt] - 2 \mu \mut + \mu \mut
  ;;   = E[Y Yt] - \mu \mut

  ;; Var Y = E[Y^2] - \mu^2


  ;; For initial estimates of covariance of \hat\beta:

  ;; \hat\beta = (Xt X)^-1 Xt Y
  ;; with E[ \hat\beta ] 
  ;;        = E[ (Xt X)^-1 Xt Y ]
  ;;        = E[(Xt X)^-1 Xt (X\beta)]
  ;;        = \beta 
  ;;        
  ;; So Var[\hat\beta] = ...
  ;;     (Xt X)
  ;; and this gives SE(\beta_i) = (* (sqrt (mref Var i i)) adjustment)


  ;; from docs:

  (setf *temp-result* 
	(let ((*default-implementation* :foreign-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    ;; no applicable conversion?
	    ;; (m-   (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) 
	    ;;       (#<FA-SIMPLE-VECTOR-DOUBLE (10 x 1)) )
	    (v- x (first (gelsy a b rcond))))))

  
  (princ *temp-result*)
  
  (setf *temp-result* 
	(let ((*default-implementation* :lisp-array))
	  (let* ((m 10)
		 (n 10)
		 (a (rand m n))
		 (x (rand n 1))
		 (b (m* a x))
		 (rcond (* (coerce (expt 2 -52) 'double-float)
			   (max (nrows a) (ncols a))))
		 (orig-a (copy a))
		 (orig-b (copy b))
		 (orig-x (copy x)))
	    (list x (gelsy a b rcond))
	    (m- x (first  (gelsy a b rcond)))
	    )))
  (princ *temp-result*)


  (defparameter *xv*
    (make-vector
     8
     :type :row ;; default, not usually needed!
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0))))

  (defparameter *y*
    (make-vector
     8
     :type :row
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-1* (m* *xv* (transpose *xv*)))
  (defparameter *xty-1* (m* *xv* (transpose  *y*)))
  (defparameter *rcond-in* (* (coerce (expt 2 -52) 'double-float)
			      (max (nrows *xtx-1*)
				   (ncols *xty-1*))))

  (defparameter *betahat*  (gelsy *xtx-1* *xty-1* *rcond-in*))

  ;;  (#<LA-SIMPLE-VECTOR-DOUBLE (1 x 1)
  ;;  1.293103448275862>
  ;;  1)

  ;;   ## Test case in R:
  ;;   x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
  ;;   y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  ;;   lm(y~x-1)
  ;;   ## => 
  ;;   Call:
  ;;   lm(formula = y ~ x - 1)

  ;;   Coefficients:
  ;;       x  
  ;;   1.293  

  (first  *betahat*))
