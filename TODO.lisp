;;; Precursor systems

;; (asdf:oos 'asdf:compile-op 'ffa :force t)
;; (asdf:oos 'asdf:compile-op 'array-operations :force t)

;; (asdf:oos 'asdf:compile-op 'org.middleangle.foreign-numeric-vector :force t)
;; (asdf:oos 'asdf:compile-op 'org.middleangle.cl-blapack :force t)

;;; The main thing...
;; (asdf:oos 'asdf:compile-op 'lisp-matrix :force t)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

;; Tests = 55, Failures = 2, Errors = 0 ;; 22.10.2008
(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))
;; failures: 
;; # ut-vectors : col-of-strided-matrix
;; # ut : make-predicate

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

;; (typep -1 '(integer 0 *))
;; (typep 2 '(integer 0 *))

;; Here is what we need to fix, based on the above:
;; #  make-predicate (and variable-capture, see macro-expansion)
;; #  col / row  on strided matrices
;; #  mref access, index assertion done better.
;; #  creation of foreign-array matrices which are integer valued
;;    fails.
;; # 




(progn
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
    (setf m2-la-int (make-matrix 2 5
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
    (setf m01b (strides m01 :nrows 2 :row-stride 2))

    (defvar m01c nil)
    (setf m01c (rand 3 4))

    (format nil "Data set up")) ; EVAL HERE TO SETUP DATA


;;;;;;; FIX ALL THE ERRORS

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
;;;
;;; and now to pull out the rows and columns via slicing on a strided
;;; matrix, we have the following approaches, for the zero-th column:
;;;     23
;;;     43
(slice *o3* :offset 0 :stride 1 :nelts (nrows *o3*) :type :column)
;;; and for the 2nd column (3rd, since we are zero counting).
;;;     25
;;;     45
(slice *o3* :offset 4 :stride 1 :nelts (nrows *o3*) :type :column)
;;; and for the 1st row (2nd, again zero-counting):
;;;     43 44 45
(slice *o3* :offset 1 :stride 2 :nelts (ncols *o3*) :type :row)
;;; 

;; strided matrix access
m01b
(orientation m01b)
(unit-strides-p m01b) ;; false, it's explicitly strided

;; slice matrix access to rows
(slice m01b :offset 0 :stride 2 :nelts (ncols m01b) :type :row)
(slice m01b :offset 1 :stride 2 :nelts (ncols m01b) :type :row)
;; slice matrix access to columns
(slice m01b :offset 0 :stride 1 :nelts (nrows m01b) :type :column)
(slice m01b :offset 2 :stride 1 :nelts (nrows m01b) :type :column)
(slice m01b :offset 4 :stride 1 :nelts (nrows m01b) :type :column)
(slice m01b :offset 6 :stride 1 :nelts (nrows m01b) :type :column)

  (m= (col m01b 0)
      (make-matrix 2 1 :initial-contents '((11d0) (31d0))))
  (m= (col m01b 1)
      (make-matrix 2 1 :initial-contents '((12d0) (32d0))))
  (m= (col m01b 2)
      (make-matrix 2 1 :initial-contents '((13d0) (33d0))))
  (m= (col m01b 3)
      (make-matrix 2 1 :initial-contents '((14d0) (34d0))))
  (m= (col m01b 4)
      (make-matrix 2 1 :initial-contents '((15d0) (35d0))))
  (row m01b 0)
  (row m01b 1)
  (col m01b 0)
  (col m01b 1)

  
  ;; FIXME: there are bugs in slicing/striding with transposed
  ;; matrices. 

  ;; the following are correct, but..
  (row m01 0)
  (row m01 1)
  (row m01 2)
  (row m01 3)

  (col m01 0)
  (col m01 1)
  (col m01 2)
  (col m01 3)

  m01
  (transpose m01)
  (row (transpose m01) 0)
  (row (transpose m01) 1) ; wrong: grab bad column, AND by 1 (pushed up)
  (row (transpose m01) 2) ; ditto, wrong by 2
  (row (transpose m01) 3) ; etc...wrong by 3

  (row (transpose m01) 0)
  (transpose (row (transpose m01) 0))

  m01
  (transpose m01)
  (col (transpose m01) 0)
  (col (transpose m01) 1) ; last rather than first
  (col (transpose m01) 2) ; completely wrong (error)
  (col (transpose m01) 3) ; ditto above


  (v= (row m01 0)
      (col (transpose m01) 0)) ;; works
  
  (m= (row m01 0)
      (col (transpose m01) 0)) ;; fails, since dims unequal
  
  m01
  (transpose m01)
  ;; given the above...
  ;; FIXME: Big Barf!
  (v= (row m01 1)
      (col (transpose m01) 1) ) ;; fails badly.  Real badly.
  
  (v= (col m01 1)
      (row (transpose m01) 1) ) ;; fails, but closer...
  
  (col m01 1)
  (col (transpose m01) 1) ;; this is the problem, indexing issue...
  
  
  ;; and the same problem.
  m3 
  (transpose m3)
  (v= (col m3 1) (row (transpose m3) 1))
  (v= (row m3 1) (col (transpose m3) 1))
	  
  ;; Striding and Slicing issues:
  ;; Strides provide matrix sections; slicing provides vector'd sections.

  ;; STRIDING
  m01
  (strides m01 :nrows 2 :row-stride 2)  ;; view just rows 1 and 3 from m01
  (strides m01 :nrows 3) ;; first 3 rows
  (strides m01 :ncols 3 :col-stride 2) ;; cols 1, 3 ,5
  (strides m01 :ncols 2) ;; first 2 cols
  m01

  ;; SLICING
  m01
  (slice m01 :offset 5 :stride  2 :nelts 3 :type :row)
  ;; col 2 
  (slice m01 :offset 5 :stride  2 :nelts 3 :type :row)


  (slice (transpose m01) :offset 5 :stride  2 :nelts 3 :type :row)
  (slice m01
	 :offset 5
	 :stride  2
	 :nelts 3
	 :type :row)
  (slice (transpose m01) :offset 5 :stride  2 :nelts 3 :type :row)

  ;; slicing isn't affected by transposition -- doesn't affect the
  ;; counting.  Would have suggested that column-major or row-major.
  ;; Should this be the case?  (need to migrate to unit-tests).

  (v=  (slice m01 :offset 5 :stride  2 :nelts 3 :type :row)
       (slice (transpose m01) :offset 5 :stride  2 :nelts 3 :type :row))
  (v=  (slice m01 :offset 5 :stride  2 :nelts 3 :type :row)
       (slice (transpose m01) :offset 5 :stride  2 :nelts 3 :type :column))
  ;; and note the above -- vector equality doesn't depend on orientation...

  (slice m01 :offset 1 :stride  2 :nelts 3 :type :column)
  (slice m01 :offset 1 :stride  0 :nelts 3 :type :column)
  ;; :type   : provides the form to provide output for
  ;; :offset : number of observations (in "col/row major"
  ;;           matrix-dependent order) to skip over before starting
  ;;           extraction
  ;; :stride : 0 = repeat same value; 1, as ordered, 2 every other, 
  ;;           etc... 


  ;; Alternative approach for slicing, via Tamas's AFFI package:
  (defparameter *my-idx* (affi:make-affi '(5 6))) ; -> generator
  (affi:calculate-index *my-idx* #(1 1)) ; -> 7 



  ;; FIXME: need to get the foriegn-friendly arrays package involved
  ;; to create integer matrices.  Or do we just throw an error that
  ;; says to use lisp-arrays?
  (make-matrix 2 5
	       :implementation :foreign-array 
	       :element-type 'integer 
	       :initial-contents #2A(( 1 2 3 4 5)
				     ( 6 7 8 9 10)))


  ;; FIXME -- indexing with mref not checked against dims, doesn't
  ;; barf correctly.  (now is checked, but badly/poorly -- this FIXME
  ;; is about better optimization, NOT about it failing to work, which
  ;; was the original problem).
  m01
  (assert-valid-matrix-index m01 1 8)
  (assert-valid-matrix-index m01 8 1)
  (mref m01 1 8) ; good -- we throw an error... but
  (mref m01 8 1) ; BAD! barfs, not protecting against first index...
  (setf (mref m01 7 7) 1.2d0)
  m01
  
  
  ;; FIXME: the following has no applicable method -- only for
  ;; doubles, not integers.  
  (m* m2 (transpose m2))
  ;; but we can multiple doubles, but...
  (m* m01 (transpose m01))


  )
