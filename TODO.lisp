n;;; Precursor systems
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

;; Tests = 65, Failures = 1, Errors = 0 ;; 11.12.2008

(describe (run-tests :suite 'lisp-matrix-ut))
(run-tests :suite 'lisp-matrix-ut)
;; or simply...
(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))

;; failures:
;; # ut         : make-predicate

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

(describe 
 (lift::run-test
  :test-case  'lisp-matrix-unittests::strided-matrix-column-access
  :suite 'lisp-matrix-ut-vectors))

;; (typep -1 '(integer 0 *))
;; (typep 2  '(integer 0 *))

;; Here is what we need to fix, based on the above:
;; #  make-predicate (and variable-capture, see macro-expansion)
;; #  col selection on row-oriented strided matrices (col-oriented fine)
;; #  mref access, index assertion done better.
;; #  creation of foreign-array matrices which are integer valued
;;    fails.
;; # 

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
    ;; would an API allowing for the following equal behaviours:
    ;;    :initial-contents (:row-major (list 1 2 3 4 5 6 7 8 9 0))
    ;;    :initial-contents (:row-major (list 1 2 3 (list 4 5 6) 7 8 9 0))
    ;; be useful for getting list-structured data into matrices?
    
    (make-matrix 2 5
		 :implementation :lisp-array  ;; :foreign-array
		 :element-type 'integer ; 'double-float
		 ;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
		 :initial-contents #2A((1 2 3 4 5)
				       (6 7 8 9 10)))
    "placeholder 2")

  ;; There is an argument for something like:
  ;;    :initial-contents (:row-major-list-lists
  ;;                           (list (list 1 2 3 4 5)
  ;;                                 (list 6 7 8 9 10)))
  ;; to exist as well.  

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
  (format nil "Data set up"))


;;;; FIX ERRORS, MIGRATE INTO UNITTESTS:

#+nil
(progn ;; FIXME: integer-valued foreign arrays. (bug: matrix-2)
  (defparameter *m2a*  (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10)))
    "placeholder 2"))

;; Foreign array problems...
#+nil
(progn ;; = FIXME: the following has no applicable method!
  (m* *m3-fa* *m3-fa*)
  (m* *m2-fa* (transpose *m2-fa*))
  (m* *m3-fa* (transpose *m3-fa*)))

#+nil
(progn ;; = FIXME: the following works (lisp-arrays)
  (m* *m3-la* *m3-la*)
  (m* *m3-la* (transpose *m3-la*)))

#+nil
(progn ;; FIXME: vectorized arithmetic
  ;;
  ;; So,  how do I vectorize something like:
  ;;     (a + b) / c  
  ;; (i.e. standard normalization) when a,b,c are vectors which have
  ;; the correct pre-computed values?  So we'd like to do something
  ;; like:
  ;;     (v./ (v.+ a b) c)
  ;; or...?  where the v.# operators disregard row vs. column oriented
  ;; aspect, and the v# operators worry about orientation.    So if we
  ;; know what we've got, we would then be able to do something like 
  ;;     (v/ (v+ a b) c)
  ;; or possibly
  ;;     (v/ (m+ a b) c)
  ;; but we still need to figure out the API for vector ops, and whether
  ;; any of this is done by BLAS (which it should be) or LAPACK.
  
  ;; On a related note, we also could have m.# instead of v.# if
  ;; orientation needs to be ensured (rather than ignored).
  (defparameter *v1* (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
  (defparameter *v2* (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
  (vector-dimension *v1*)
  (v+ *v1* *v2*)
  (v- *v1* *v2*)
  (v- *v2* *v1*)
  (v* *v1* *v2*)
  (v/ *v1* *v2*)
  (v/ *v2* *v1*)

  (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
	 (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	 (c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0)))))
    (v= (v+ a b)
	c)
    (v= (v+ b a)
	c))

  (defparameter a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
  (defparameter b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
  (defparameter c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0))))
  (v= (v+ a b)
      c)
  (v= (v+ b c)
      c)

  (princ "vector ops done."))

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

  (list-of-rows m01)
  
  (mapcar #'princ (list-of-columns m01))
  )


#+nil
(progn  ;;; QR factorization
  ;; Need to incorporate the xGEQRF routines, to support linear
  ;; regression work.   

  ;; Some issues exist in the LAPACK vs. LINPACK variants, hence R
  ;; uses LINPACK primarily, rather than LAPACK.  See comments in R
  ;; source for issues.  
  
  ;; LAPACK suggests to use the xGELSY driver (GE general matrix, LS
  ;; least squares, need to lookup Y intent (used to be an X alg, see
  ;; release notes).

  ;; Goal is to start from X, Y and then realize that if
  ;; Y = X \beta, then,   i.e. 8x1 = 8xp px1  + 8x1
  ;;      XtX \hat\beta = Xt Y
  ;; so that we can solve the equation  W \beta = Z   where W and Z
  ;; are known, to estimate \beta.
  (defparameter *xv*
    (make-vector
     8
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0))))

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

  (defparameter *xm*
    (make-matrix
     2 8
     :initial-contents '((1d0 3d0 2d0 4d0 3d0 5d0 4d0 6d0)
			 (1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  (defparameter *y*
    (make-vector
     8
     :initial-contents '((1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))))

  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx* (m* *xv* (transpose *xv*)))
  (defparameter *xty* (m* *xv* (transpose  *y*)))
  (defparameter *rcond* 1)
  (defparameter *betahat*  (gelsy *xtx* *xty* *rcond*))
  *betahat*

#|
(#<LA-SIMPLE-VECTOR-DOUBLE (1 x 1)
 1.293103448275862>
 1)
|#

#|

## Test case in R:
x <- c( 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0)
y <- c( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
lm (y ~ x  -1)
## => 
Call:
lm(formula = y ~ x - 1)

Coefficients:
    x  
1.293  
|#


  ;; so something like (NOTE: matrices are transposed to begin with, hence the incongruety)
  (defparameter *xtx* (m* *xv+1* (transpose *xv+1*)))
  (defparameter *xty* (m* *xv+1* (transpose  *y*)))
  (defparameter *rcond* 1)
  (defparameter *betahat*  (gelsy *xtx* *xty* *rcond*))
  *betahat*



  ;; which suggests one might do (modulo ensuring correct orientations)
  (defun lm (x y)
    (let ((betahat (gelsy (m* x (transpose x))
			  (m* x (transpose y)))))
      
      (values betahat (sebetahat betahat x y))))
  ;; to get a results list containing betahat and SEs

  (values-list '(1 3 4))
  )