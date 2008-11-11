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

;; Tests = 56, Failures = 1, Errors = 0 ;; 27.10.2008
(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))
;; failures:
;; # ut         : make-predicate

;; Tests to create:  
;; # Need to add tests for col/row-offset access for general/all matrices. 

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

  (defvar m01 nil
    "6x5 matrix with entries representing row+1,col+1 values, for
     test purposes.")

  (setf m01 (make-matrix
	     6 5
	     :initial-contents '((11d0 12d0 13d0 14d0 15d0)
				 (21d0 22d0 23d0 24d0 25d0)
				 (31d0 32d0 33d0 34d0 35d0)
				 (41d0 42d0 43d0 44d0 45d0)
				 (51d0 52d0 53d0 54d0 55d0)
				 (61d0 62d0 63d0 64d0 65d0))))

  (defvar m1-ex nil
    "quick variable initialized to zeros")
  (setf m1-ex (make-matrix 2 5
			   :implementation :lisp-array  ;; :foreign-array
			   :element-type 'double-float))
    
  (defvar m2-la-int nil
    "placeholder 2")
  ;; would an API allowing for the following equal behaviours:
  ;;    :initial-contents (:row-major (list 1 2 3 4 5 6 7 8 9 0))
  ;;    :initial-contents (:row-major (list 1 2 3 (list 4 5 6) 7 8 9 0))
  ;; be useful for getting list-structured data into matrices?
  (setf m2-la-int
	(make-matrix 2 5
		     :implementation :lisp-array  ;; :foreign-array
		     :element-type 'integer ; 'double-float
		     ;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
		     :initial-contents #2A((1 2 3 4 5)
					   (6 7 8 9 10))))
  ;; There is an argument for something like:
  ;;    :initial-contents (:row-major-list-lists
  ;;                           (list (list 1 2 3 4 5)
  ;;                                 (list 6 7 8 9 10)))
  ;; to exist as well.  

  ;; Currently we can make a foriegn matrix of doubles, but not a
  ;; foriegn matrix of integers.
  (defvar m2-fa nil
    "placeholder 2")
  (setf m2-fa
	(make-matrix
	 2 5
	 :implementation :foreign-array 
	 :element-type 'double-float
	 :initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
			       ( 6d0 7d0 8d0 9d0 10d0))))
    

  (defvar m01b nil)
  (setf m01b (strides m01 :nrows 2 :ncols 3
		      :row-stride 2
		      :row-offset 1 :col-offset 1))
  
  (defvar m01c nil)
  (setf m01c (window m01
		     :nrows 2 :ncols 3
		     :row-offset 2 :col-offset 1))

  (format nil "Data set up")) ; EVAL HERE TO SETUP DATA




(progn ;; == FIX ALL THE ERRORS

  ;; = FIXME: need to get the foriegn-friendly arrays package involved. 
  (defvar m2a nil
    "placeholder 2")
  (setf m2a (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10))))


  ;; = FIXME: the following has no applicable method!
  (m* m2 (transpose m2))1

  ;; = FIXME: ...
  )

(defparameter *m1*  (zeros 2 3))
(defparameter *m2*  (zeros 2 2))
(defparameter *m3*  (zeros 3 2))

*m1*  *m2*  *m3*
(bind2 *m1* *m2* :by :column) ;; works
(bind2 *m2* *m3* :by :row) ;; fails
