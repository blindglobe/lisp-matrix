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

;; Tests = 68, Failures = 1, Errors = 2 ;; 26.2.2009

(run-tests :suite 'lisp-matrix-ut)
(describe (run-tests :suite 'lisp-matrix-ut))
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


;;; FIXME FOLLOWING ERRORS: MIGRATE INTO UNITTESTS...

(progn  ;;#FIXME: factorization and inversion via LAPACK

  (defparameter *eye* (eye 7 7))
  (m*  (minv-lu  *eye*) *eye*)

  (matrix-like-symmetric-p (rand 4 4))
  (let ((myrand (rand 4 4)))
    (matrix-like-symmetric-p (m* (transpose myrand) myrand)))

  (defparameter *rand* (rand 4 4))
  (defparameter *symrand* (m* (transpose *rand*) *rand*))
  (matrix-like-symmetric-p *symrand*)
  (defparameter *symrand-copy* (copy *symrand*))
  (m* *symrand* (minv-cholesky *symrand*))

  (min (values (list 4d0 2d0 3d0 5d0 3d0)))
  (reduce #'min (list 4d0 2d0 3d0 5d0 3d0))

  (defparameter *n* 3) ; # rows = # obsns
  (defparameter *p* 5) ; # cols = # vars 
  (defparameter *x-temp*  (rand *n* *p*))
  (defparameter *b-temp*  (rand *p* 1))
  (defparameter *y-temp*  (m* *x-temp* *b-temp*))
  ;; so Y=Xb + \eps
  (defparameter *rcond* (* (coerce (expt 2 -52) 'double-float)
		   (max (nrows *x-temp*) (ncols *y-temp*))))
  (defparameter *orig-x* (copy *x-temp*))
  (defparameter *orig-b* (copy *b-temp*))
  (defparameter *orig-y* (copy *y-temp*))

  (defparameter *xtx-temp* (m* (transpose *x-temp*) *x-temp*))
  (defparameter  *xtx-temp-i* (minv-cholesky  *xtx-temp*))
  (defparameter *is-it-i* (m* *xtx-temp-i* *xtx-temp*))


  (reduce #'(lambda (x y) (concatenate 'string x y))
	  "test"
	  " "
	  (list "a2" " s3 " "asdf")
	  "end.")
	  

  (defun lispmatrix2r (m &key (rvarname "my.mat"))
    "Write out a string that can be used to read in the matrix into R.
Used for creating verfication scripts and test cases."
    (check-type m matrix-like)
    (apply 
     #'concatenate 'string
     (format nil "~%~s <- matrix ( data = c(" rvarname)
     (let ((result (list)))
		    (dotimes (i (matrix-dimension m 0))
		      (dotimes (j (matrix-dimension m 1))
			(cons (format nil "~d," (mref m i j)) result)))
		    (reverse result))
     (list  (format nil "), nrows=~d, ncols=~d, by.row=TRUE)"
	     (matrix-dimension m 0)
	     (matrix-dimension m 1)))))

  (lispmatrix2R *x-temp*)



  (concatenate 'string
	       (format nil "~%~s <- matrix ( data = c(" "testme")
	       (values-list (list "test" "test" "test")))

  (apply #'concatenate  'string (list "test" "test" "test"))

  (let ((result (make-array (list 3 5) :element-type 'string)))
    (dotimes (i 3)
      (dotimes (j 5)
	(format t "~s ~s ~%" i j)
	(setf (aref result i j) (format t "(~d ~d)," i j))))
    (reverse result))


  (let* ((state1 (make-random-state))
	 (state2 (make-random-state state1)))
    (m= (rand 2 3 :state state1)
	(rand 2 3 :state state2)))


  
  (defparameter *basic-mat* (rand 3 3)) ; start with a square matrix
  (defparameter *potri-test1*
    (m* (transpose *basic-mat*) *basic-mat*)) ; symmetrize via XtX
  (defparameter *potri-test2* (copy *potri-test1*)) ; save a copy
  (potri (first  (potrf *potri-test1*))) ; factor and then invert
  (defparameter *porti-test3 (trap2mat *potri-test1*)) ; un-triangulize
  (m* *potri-test1* *potri-test2*) ; test for inversion


  ;;; Problems here...
  (geqrf (make-matrix 2 2 :initial-contents #2A(( 1d0 2d0 ) (2d0 1d0)))
	 (make-vector 2 :type :column :initial-contents '((1d0)(1d0))))

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
	    (r  (if (weights lm-obj)
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

  (princ "Linear Models Code setup"))




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

  ;; We would need such an "extractor" to make things work out right.  
  (mapcar #'function-on-matrix (make-list-of-matrices original-matrix)) 


  (list->vector-like (list 1d0 2d0 3d0) :orientation :row)

  (make-vector 3 :type :column 
	       :initial-contents
	       (mapcar #'(lambda (x) (list (coerce x 'double-float)))
		       (list 1d0 2d0 3d0)))

  (make-vector 3 :type :row 
	       :initial-contents
	       (list (mapcar  #'(lambda (x) (coerce x 'double-float))
			      (list 1d0 2d0 3d0))))

  ;; The following approach would be required to do a proper map-back.
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

  (format nil "R-Apply approach"))


#+nil
(progn
  ;; Studies in Class inheritance

  (subtypep 'LA-SIMPLE-VECTOR-DOUBLE 'VECTOR-LIKE)
  (subtypep 'LA-SLICE-VECVIEW-DOUBLE 'VECTOR-LIKE)
  (subtypep 'LA-SIMPLE-VECTOR-DOUBLE 'LA-SLICE-VECVIEW-DOUBLE)
  (subtypep  'LA-SLICE-VECVIEW-DOUBLE 'LA-SIMPLE-VECTOR-DOUBLE)

  (subtypep 'FA-SIMPLE-VECTOR-DOUBLE 'MATRIX-LIKE)

  ;;; weird!
  (m- (make-vector 2 :initial-contents '((1d0 1d0)))
      (make-vector 2 :initial-contents '((1d0 1d0))))

  (let ((*default-implementation* :foreign-array))
    (m- (make-vector 2 :initial-contents '((1d0 1d0)))
	(make-vector 2 :initial-contents '((1d0 1d0)))))

  (let ((*default-implementation* :lisp-array))
    (m- (make-vector 2 :initial-contents '((1d0 1d0)))
	(make-vector 2 :initial-contents '((1d0 1d0)))))

  (m- (make-vector 2
		   :implementation :lisp-array
		   :initial-contents '((1d0 1d0)))
      (make-vector 2
		   :implementation :foreign-array
		   :initial-contents '((1d0 1d0))))

  (typep  (first *lm-result*) 'vector-like)
  (typep  (first *lm-result*) 'matrix-like)
  (typep  (second *lm-result*) 'vector-like)
  (typep  (second *lm-result*) 'matrix-like)
  (typep *x-temp* 'vector-like)
  (typep *x-temp* 'matrix-like) ; => T ,rest of this paragraph are false.

  (m- *x-temp* *x-temp*))
