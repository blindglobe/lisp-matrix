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

;; Tests = 59, Failures = 1, Errors = 0 ;; 11.12.2008

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
    
  (defparameter *m01b*
    (strides m01 :nrows 2 :ncols 3
	     :row-stride 2
	     :row-offset 1 :col-offset 1))
  
  (defparameter *m01c* 
    (window m01
	    :nrows 2 :ncols 3
	    :row-offset 2 :col-offset 1))

  (format nil "Data set up")) ; EVAL HERE TO SETUP DATA


;;;; FIX ERRORS, MIGRATE INTO UNITTESTS:

#+nil
(progn ;; FIXME: need to get the foriegn-numeric-vector arrays package
       ;; involved for integer-valued foreign arrays.
  (defparameter *m2a*  (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10)))
    "placeholder 2"))



#+nil
(progn ;; = FIXME: the following has no applicable method!
  (m* *m2-fa* (transpose *m2-fa*)))


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
    (v= (v+ b c)
	c))

  (defparameter a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
  (defparameter b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
  (defparameter c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0))))
  (v= (v+ a b)
      c)
  (v= (v+ b c)
      c))

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
