;;; Precursor systems
(in-package :cl-user)
;; (asdf:oos 'asdf:compile-op 'ffa :force t)
;; (asdf:oos 'asdf:compile-op 'array-operations :force t)

;; (asdf:oos 'asdf:compile-op 'org.middleangle.foreign-numeric-vector :force t)
;; (asdf:oos 'asdf:compile-op 'org.middleangle.cl-blapack :force t)

;;; The main thing...
;; (asdf:oos 'asdf:compile-op 'lisp-matrix :force t)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

;; Tests = 65, Failures = 0, Errors = 0 ;; 9.2.2009

(describe (run-tests :suite 'lisp-matrix-ut))
(run-tests :suite 'lisp-matrix-ut)
;; or simply...
(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))

;; failures:

;; Note that when unit tests fail in m*- tests, it seems to do with a
;; "macro vs defun" problem, related to compile-time vs. run-time
;; evaluation that I (tony) am not quite getting.

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

(describe 
 (lift::run-test
  :test-case  'lisp-matrix-unittests::strided-matrix-column-access
  :suite 'lisp-matrix-ut-vectors))


;; Here is what we need to fix, based on the above:
;; #  creation of foreign-array matrices which are integer valued
;;    fails.


;; Just a reminder:
;; (typep -1 '(integer 0 *))
;; (typep  2 '(integer 0 *))

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
  (format nil "Data set up"))

;;;; FIX ERRORS, MIGRATE INTO UNITTESTS:


#+nil
(progn ;; FIXME: integer-valued foreign arrays. (bug: matrix-2)
  (defparameter *m2a*  (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10)))
    "test for integer-valued foreign arrays"))

;; Foreign array problems...
#+nil
(progn ;; = FIXME: the following has no applicable method!
  (m* *m3-fa* *m3-fa*)
  (m* *m2-fa* (transpose *m2-fa*))
  (m* *m3-fa* (transpose *m3-fa*)))


(progn ;; = SOLVED: the following works (lisp-arrays)
  (m* *m3-la* *m3-la*)
  (m* *m3-la* (transpose *m3-la*)))


#+nil
(progn ;; FIXME: R's apply across array indicies

  ;; Thought 1 (currently not planned for implementation)
  ;; consider using affi as a general iterator/walker generator.
  ;; So, R has a notion of apply, sapply, tapply, lapply -- what we
  ;; should do is something like
  ;;
  ;;     (map-matrix with-fn this-matrix
  ;;                 :by iterator
  ;;                 :result-type 'list)
  ;;
  ;; silly or for later:        :computation-type [:parallel|:serial]
  ;;
  ;; or similar, where :result-type is something that can be coerced to
  ;; from a sequence, and computation-type might drive whether there are
  ;; dependencies or not.   (this last is probably too premature).

  ;; The basic idea is to use vector functions (taking a vector, and
  ;; returning a object) and use them to provide an object that can be
  ;; part of a list (or generally, a sequence of homogeneous objects).

  ;; Reviewing Tamas Papp's affi package provides one approach to this
  ;; challenge.  He suggests that an obvious approach would be to
  ;; break up the 2 actions needed for selection consist of describing
  ;; the mapping from array to structure, and then walking the
  ;; structure to extract (for copy or use).  For our needs, we need a
  ;; means of doing this to partition the space, and then
  ;; post-partition, deciding which partitions need to be considered
  ;; for further processing, and which ones get discarded.

  ;; So to clarify how this might work: 
  ;; 1. we need a function which takes a matrix and creates a list of
  ;; matrix-like or vector-like elements.
  ;; 2. we have functions which operate in general on matrix-like or
  ;; vector-like objects.
  ;; 3. we use mapcar or similar to create the results.  
  ;; 3a. multi-value return could be used to create multiple lists of
  ;; vector-like or matrix-like objects, for example to get a complex
  ;; computation using inner-products.   So for instance:
  ;;   list1: v1a v2a v3a
  ;;   list2: m1  m2  m3
  ;;   list3: v1b v2b v3b
  ;; and we compute
  ;;   (list-of (IP v#a m1 v#b )) 
  ;; via
  ;;   (mapcar #'IP (list-of-vector-matrix-vector M))

  (mapcar #'function-on-matrix (make-list-of-matrices original-matrix)) 
  (list->vector-like (map 'list #'function-of-2-args (list1) (list2))
		     :type :row) ; or :column
  ;; this would take a list and create an appropriate vector-like of
  ;; the appropriate type.

  ;; Thought 2, the current immediate approach:
  ;; What we currently do is break it out into components.
  (list-of-columns m01)

  (defparameter *m1-app* (ones 2 3))
  (let ((col-list (list-of-columns *m1-app*)))
    (dotimes (i (length col-list))
	  (princ (v= (nth i col-list)
		      (ones 2 1)))))

  (list-of-rows *m01*)
  
  (mapcar #'princ (list-of-columns *m01*))
  )


#+nil
(progn  ;;; QR factorization
  ;; Need to incorporate the xGEQRF routines, to support linear
  ;; regression work.   

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
	    (list x (gelsy a b rcond)))))
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
	    (list x (gelsy a b rcond)))))
  (princ *temp-result*)

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-1* (m* *xv* (transpose *xv*)))
  (defparameter *xty-1* (m* *xv* (transpose  *y*)))
  (defparameter *rcond-in* (* (coerce (expt 2 -52) 'double-float)
			      (max (nrows *xtx-1*)
				   (ncols *xty-1*))))

  (defparameter *betahat*  (gelsy *xtx-1* *xty-1* *rcond-in*))
  *betahat*

#|
 (#<LA-SIMPLE-VECTOR-DOUBLE (1 x 1)
 1.293103448275862>
 1)


  ## Test case in R:
  x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
  y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  lm(y~x-1)
  ## => 
  Call:
  lm(formula = y ~ x - 1)

  Coefficients:
      x  
  1.293  
|#

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx-2* (m* (transpose *xv+1*) *xv+1*))
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  8.0d0 28.0d0
  ;;  28.0d0 116.0d0>

  (defparameter *xty-2* (m* (transpose *xv+1*)  (transpose *y*)))
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  36.0d0
  ;;  150.0d0>

  (defparameter *rcond-2* 0.000001)
  (defparameter *betahat-2*  (gelsy *xtx-2* *xty-2* *rcond-2*))
  ;; *xtx-2* => "details of complete orthogonal factorization"
  ;; according to man page:
  ;; #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
  ;;  -119.33147112141039d0 -29.095426104883202d0
  ;;  0.7873402682880205d0 -1.20672274167718d0>

  ;; *xty-2* => output becomes solution:
  ;; #<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
  ;;  -0.16666666666668312d0
  ;;  1.333333333333337d0>

  *betahat-2* ; which matches R, see below

  (documentation 'gelsy 'function)
#|

  (#<LA-SIMPLE-VECTOR-DOUBLE (2 x 1)
   -0.16666666666668312 1.333333333333337>
   2)

  ## Test case in R:
  x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
  y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  lm(y~x)
  ## => Call:  lm(formula = y ~ x)

  Coefficients:  (Intercept)            x  
                     -0.1667       1.3333  

  summary(lm(y~x))
  ## =>

  Call:
  lm(formula = y ~ x)

  Residuals:
         Min         1Q     Median         3Q        Max 
  -1.833e+00 -6.667e-01 -3.886e-16  6.667e-01  1.833e+00 

  Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
  (Intercept)  -0.1667     1.1587  -0.144  0.89034   
  x             1.3333     0.3043   4.382  0.00466 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

  Residual standard error: 1.291 on 6 degrees of freedom
  Multiple R-squared: 0.7619,	Adjusted R-squared: 0.7222 
  F-statistic:  19.2 on 1 and 6 DF,  p-value: 0.004659 

|#

  ;; which suggests one might do (modulo ensuring correct
  ;; orientations).  When this is finalized, it should migrate to
  ;; CLS.
  ;;
  ;; might add args: (method 'gelsy), or do we want to put a more
  ;; general front end, linear-least-square, across the range of
  ;; LAPACK solvers? 
  (defun lm ( x y)
    "fit the linear model:
           y = x \beta + e 

and estimate \beta.  X,Y should be in cases-by-vars form, i.e. X
should be n x p, Y should be n x 1.  Returns estimates, n and p.
Probably should return a form providing the call, as well.

R's lm object returns: coefficients, residuals, effects, rank, fitted,
qr-results for numerical considerations, DF_resid.  Need to
encapsulate into a class or struct.
"
    (check-type x matrix-like)
    (check-type y vector-like) ; vector-like might be too strict?
					; maybe matrix-like?
    (assert (= (nrows y) (nrows x)) ; same number of observations/cases
	    (x y) "Can not multiply x:~S by y:~S" x y)
    (let ((betahat (gelsy (m* (transpose x) x)
 		 	  (m* (transpose x) y)
			  (* (coerce (expt 2 -52) 'double-float)
			     (max (nrows x)
				  (ncols y))))))
      ;; need computation for SEs, 

      (values betahat 
	      ;;(sebetahat betahat x y)
	      (nrows x) ; surrogate for n
	      (ncols x)))) ; surrogate for p


  (defun XtXinv (x)
    "(XtX)^-1 as XtX is PxN, so whole is PxP.  Usually represents the
   Vars for beta from Y = X \beta + \eps.  Uses LAPACK's dpotri
   routine to invert, after using dpotrf to factorize.  We use a copy
   for now, until we understand whether or not it is safe to destroy.
   Perhaps have a destructive version of this?"
    (let (a (copy x))
      (potri (potrf (m* (transpose a) a))))) ; invert symmetric matrix

  (let* ((state1 (make-random-state))
	 (state2 (make-random-state state1)))
    (m= (rand 2 3 :state state1)
	(rand 2 3 :state state2)))


  (defparameter *basic-mat* (rand 3 3))
  (defparameter *potri-test1*
    (m* (transpose *basic-mat*) *basic-mat*))
  (defparameter *potri-test2* (copy *potri-test1*))
  (potri (potrf *potri-test1*))
  (setf (mref *potri-test1* 1 0) (mref *potri-test1* 0 1))
  (setf (mref *potri-test1* 2 0) (mref *potri-test1* 0 2))
  (setf (mref *potri-test1* 2 1) (mref *potri-test1* 1 2))
  (m* *potri-test1* *potri-test2*)



  (geqrf (make-matrix 2 2 :initial-contents #2A( ( 1d0 2d0 ) (2d0 1d0)))
	 (make-vector 2 :initial-contents '((1d0 1d0))))

  (defun print-lm (lm-obj)
    "transcribed from R"
    (p (rank lm-obj)
    (when (= p 0) 
      ;; EVIL LOGIC!  Just to store for now.
      (let ()
	    (n (length (residuals lm-obj)))
	    (w (if (weights lm-obj)
		   (weights lm-obj)
		   (ones n 1)))
	    (r  (if (weights lm-obj)
		      (residuals lm-obj)
		      (v.* (residuals lm-obj)
			   (mapcar #'sqrt (weights lm-obj)))))
	    (rss (sum (v.* r r)))
	    (resvar (/ rss (- n p)))
	    ;; then answer, to be encapsulated in a struct/class
	    ;; instance, 
	    (aliased (is.na (coef lm-obj)))
	    (residuals r)
	    (df (list 0 n (length aliased)))
	    (coefficients (list 'NA 0d0 4d0))o
	    (sigma (sqrt resvar))
	    (r.squared 0d0)
	    (adj.r.squared 0d0)))
      )
    ;;otherwise...
    (when (not (= p 0))
      (let ((n (nrows (qr lm-obj)))
	    (rdf  (- n p))
	    ))))

  (lm *xv+1* *y2*)
  (lm (transpose *xv*) *y2*)

  (format nil "Linear Models Code setup")
  )
