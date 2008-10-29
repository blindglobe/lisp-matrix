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

;; Tests = 56, Failures = 2, Errors = 0 ;; 27.10.2008
(run-lisp-matrix-tests)
(describe  (run-lisp-matrix-tests))
;; failures:
;; # ut-vectors : col-of-strided-matrix
;; # ut         : make-predicate

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

;; (typep -1 '(integer 0 *))
;; (typep 2  '(integer 0 *))

;; Here is what we need to fix, based on the above:
;; #  make-predicate (and variable-capture, see macro-expansion)
;; #  col selection on strided matrices
;; #  mref access, index assertion done better.
;; #  creation of foreign-array matrices which are integer valued
;;    fails.
;; # 

(lift::run-test :test-case  'lisp-matrix-unittests::strided-matrix-column-access
		:suite 'lisp-matrix-ut-vectors)

(progn 
(defparameter *a*
  (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
				       (6d0  7d0  8d0  9d0  10d0)
				       (11d0 12d0 13d0 14d0 15d0)
				       (16d0 17d0 18d0 19d0 20d0)
				       (21d0 22d0 23d0 24d0 25d0)
				       (26d0 27d0 28d0 29d0 30d0))))
(defparameter *b* (strides *a* :nrows 3 :row-stride 2))
(defparameter *c* (window *a* :nrows 3 :row-offset 3))
(defparameter *d* (window *a* :nrows 3 :ncols 2 :row-offset 3 :col-offset 2))
(format nil "Data initialized")
)
(orientation *b*)

;; Striding
(typep *b* 'lisp-matrix::strided-matview)
(typep *b* 'lisp-matrix::window-matview)
(typep *b* 'strided-matview)
(typep *b* 'window-matview)

(parent *b*)
(offset *b*) (offset *a*)
(row-offset *a*) (col-offset *a*) ;; FIXME: we should return 0, not throw error.
(row-offset *b*) (row-offset *b*)
(row-offset *c*) (row-offset *c*)
(col-stride *b*)  (row-stride *b*) (nrows (parent *b*))
(m= (col *b* 0)
    (make-matrix 3 1 :initial-contents '((1d0) (11d0) (21d0))))
(m= (col *b* 1) ;; wrong!
    (make-matrix 3 1 :initial-contents '((2d0) (12d0) (22d0))))
(m= (col *b* 2)
    (make-matrix 3 1 :initial-contents '((3d0) (13d0) (23d0))))
(m= (col *b* 3)
    (make-matrix 3 1 :initial-contents '((4d0) (14d0) (24d0))))
(m= (col *b* 4)
    (make-matrix 3 1 :initial-contents '((5d0) (15d0) (25d0))))







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
    (setf m01b (strides m01 :nrows 2 :ncols 4
			:row-stride 2
			:row-offset 1 :col-offset 1))

    (defvar m01c nil)
    (setf m01c (window m01
		       :nrows 2 :ncols 3
		       :row-offset 2 :col-offset 1))

    (format nil "Data set up")) ; EVAL HERE TO SETUP DATA

;;;;;;; FIX ALL THE ERRORS

;; strided matrix col access
m01b
(orientation m01b)
(unit-strides-p m01b) ;; false, it's explicitly strided
(parent m01b)
(orientation  (parent m01b))
(unit-strides-p (parent m01b)) ;; true, it's the original...

;; Windowed matrix
(orientation m01c)
(row m01c 0) ; Y
(row m01c 1) ; Y
(col m01c 0) ; Y
(col m01c 1) ; Y
(col m01c 2) ; Y

;; slice matrix access to rows
(row m01b 0) ; Y
(row m01b 1) ; Y
(orientation m01b) (offset m01b)
(row-offset m01b) (col-offset m01b)
(col m01b 0) ; N
(col m01b 1) ; N...
(col m01b 2)
(col m01b 3)

(slice m01b :offset 0 :stride 2 :nelts (ncols m01b) :type :row)
(slice (parent m01b) ; equiv on parent
       :offset 1
       :stride 2
       :nelts (ncols m01b)
       :type :row)
;; 
(slice m01b :offset 1 :stride 2 :nelts (ncols m01b) :type :row)
(slice (parent m01b) ; equiv on parent
       :offset 1
       :stride 2
       :nelts (ncols m01b)
       :type :row)

;; slice matrix access to columns
(slice m01b :offset 0 :stride 1 :nelts (nrows m01b) :type :column)
(col m01b 0)
(slice m01b :offset 2 :stride 1 :nelts (nrows m01b) :type :column)
(col m01b 1)
(slice m01b :offset 4 :stride 1 :nelts (nrows m01b) :type :column)
(col m01b 2)
(slice m01b :offset 6 :stride 1 :nelts (nrows m01b) :type :column)
(col m01b 3)
(offset m01b)
(row-stride m01b) ; => 2
(col-stride m01b) ; => 1

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
