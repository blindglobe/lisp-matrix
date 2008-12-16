;;; This file illustrates some common actions in the course of working
;;; with matrices using lisp-matrix.  It is important to note that
;;; there are better ways to do this, that this are to help introduce
;;; usage, not describe best practices for using this system.

;;; = Precursor systems
;;  (asdf:oos 'asdf:compile-op 'ffa :force t)
;;  (asdf:oos 'asdf:compile-op 'org.middleangle.foreign-numeric-vector :force t)
;;  (asdf:oos 'asdf:compile-op 'org.middleangle.cl-blapack :force t)

;;; = The maing thing...
;; (asdf:oos 'asdf:compile-op 'lisp-matrix :force t)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)

;;; And the only thing that ought to be required;
(asdf:oos 'asdf:load-op 'lisp-matrix)

;;; Check status of the installation...

(in-package :lisp-matrix-unittests)
(run-lisp-matrix-tests)

;; if the above describes errors, here is how we figure out what bug
;; report to write...

(describe  (run-lisp-matrix-tests))

;;; Now we can use it, either by importing the symbols into the
;;; current package by:

;; (use-package :lisp-matrix)

;;; or by trying it out in the -user package, before implementing for
;;; production usage.

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))

;;; We wrap these up into a progn for simple overall evaluation, but
;;; stepping through them is fine as well.

(progn 
  
  ;; make some matrices
  (defparameter *m1* (make-matrix 2 5
			:implementation :lisp-array  ;; :foreign-array
			:element-type 'double-float)
    "placeholder 1")
  
  ;; works, as it should.  Indexing is zero-based, so we get the first
  ;; element by...
  (mref *m1* 0 0)
  (mref *m1* 1 3)
  (setf (mref *m1* 1 3) 1.2d0)
  *m1*


  ;; increase complexity

  (defparameter *m2* (make-matrix 2 5
			:implementation :lisp-array  ;; :foreign-array
			:element-type 'integer ; 'double-float
			;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10)))
    "placeholder 2")

  (defparameter *m2a*
    (make-matrix 2 5
		 :implementation :lisp-array  ;; :foreign-array
		 :element-type 'integer ; 'double-float
		 :initial-contents '((1 2 3 4 5)
				     (6 7 8 9 10)))
    "placeholder...")

  ;; Currently we can make a foriegn matrix of doubles, but not a
  ;; foreign matrix of integers.  If we are working with smaller
  ;; matrices and are not doing a great deal of matrix algebra, then
  ;; we probably prefer :lisp-array rather than :foreign-array.
  (defvar *m2b*
    (make-matrix 2 5
		 :implementation :foreign-array 
		 :element-type 'double-float
		 :initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
				       ( 6d0 7d0 8d0 9d0 10d0)))
    "placeholder 2")
  *m2b*

  (mref *m2b* 0 2) ;; => 3
  *m2b*
  (transpose *m2b*)

  ;; simple subsetting is simple
  (m= (row *m2b* 0)
      (col (transpose *m2b*) 0)) ; => nil, orientation
  (v= (row *m2b* 0)
      (col (transpose *m2b*) 0)) ; => T, no orientation worries

  (m= (col *m2b* 0)
      (row (transpose *m2b*) 0))
  (v= (col *m2b* 0)
      (row (transpose *m2b*) 0))


  (defvar *m3*
    (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					 (6d0  7d0  8d0  9d0  10d0)
					 (11d0 12d0 13d0 14d0 15d0)
					 (16d0 17d0 18d0 19d0 20d0)
					 (21d0 22d0 23d0 24d0 25d0)
					 (26d0 27d0 28d0 29d0 30d0)))
    "placeholder 3")

  (row *m3* 2)
  (col *m3* 1)


  (= (mref *m3* 0 1)
     (mref (transpose *m3*) 1 0))

  (=  (mref *m3* 2 2)
      (mref (transpose *m3*) 2 2))

  *m3*
  (transpose *m3*)

  ;;; Now we play with striding and slicing subsets.  These work well
  ;;; for simple subsetting which can be done by counting/enumeration
  ;;; on some form of regular scale.

  ;;; In addition, equality is somewhat important for numerical
  ;;; issues.  Right.  Anyway, for matrices it is mostly clear what to
  ;;; do, but for vectors, which are inheriting from matrices, we have
  ;;; 2 issues.  The first is the obvious, the numerical values, and
  ;;; the second is not quite obvious, which is the metadata
  ;;; surrounding the difference between an MxN and NxM matrix.  For
  ;;; the first, think about v= and for the second, m= is the right
  ;;; function.

  (defvar *m4* (strides *m3* :nrows 2 :row-stride 2)
    "yet another placeholder.")
  *m4*
  (m= (row *m4* 0)
      (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0))))
  (m= (row *m4* 1)
      (make-matrix 1 5 :initial-contents '((11d0 12d0 13d0 14d0 15d0))))
  ;; note the redoing for the columns -- different!
  (m= (col *m4* 0)
      (make-matrix 2 1 :initial-contents '((1d0) (11d0))))
  (m= (col *m4* 1)
      (make-matrix 2 1 :initial-contents '((2d0) (12d0))))

  (v= (row *m4* 0) (col (transpose *m4*) 0))
  (v= (col *m4* 0) (row (transpose *m4*) 0))

  *m4*
  (row *m4* 0)
  (col *m4* 4)


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


;;;;;;;;


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
  (col (transpose m01) 2) ;
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






(progn 
  (defparameter *a*
    (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					 (6d0  7d0  8d0  9d0  10d0)
					 (11d0 12d0 13d0 14d0 15d0)
					 (16d0 17d0 18d0 19d0 20d0)
					 (21d0 22d0 23d0 24d0 25d0)
					 (26d0 27d0 28d0 29d0 30d0))))
  (defparameter *b* (strides *a* :nrows 3 :row-stride 2))
  (defparameter *b1* (strides *a* :nrows 2 :ncols 3 :row-stride 2 :col-stride 1))
  (defparameter *c* (window *a* :nrows 3 :row-offset 3))
  (defparameter *d* (window *a* :nrows 3 :ncols 2 :row-offset 3 :col-offset 2))
  (format nil "Data initialized"))

(orientation *b*)

;; Striding
(typep *b* 'lisp-matrix::strided-matview)
(typep *b* 'lisp-matrix::window-matview)
(typep *b* 'strided-matview)
(typep *b* 'window-matview)

(parent *b*)
(offset *b*) (offset *a*)
(row-offset *a*) (col-offset *a*)
(row-offset *b*) (col-offset *b*)
(row-offset *c*) (row-offset *c*)
(col-stride *b*)  (row-stride *b*) (nrows (parent *b*))

(equal  (data *a*)
	(data *b*))
;; col 0 =  1  3  5 indicies; currently getting  1 13 25  (+ 12, not + 2)
;; col 1 =  7  9 11 indicies
;;
(m= (princ  (col *b* 0))
    (princ  (make-matrix 3 1 :initial-contents '((1d0) (11d0) (21d0)))))
(m= (col *b* 1) 
    (make-matrix 3 1 :initial-contents '((2d0) (12d0) (22d0))))
(m= (col *b* 2)
    (make-matrix 3 1 :initial-contents '((3d0) (13d0) (23d0))))
(m= (col *b* 3)
    (make-matrix 3 1 :initial-contents '((4d0) (14d0) (24d0))))
(m= (col *b* 4)
    (make-matrix 3 1 :initial-contents '((5d0) (15d0) (25d0))))
