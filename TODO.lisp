;;; Precursor systems

;;  (asdf:oos 'asdf:compile-op 'ffa :force t)

;;  (asdf:oos 'asdf:compile-op 'org.middleangle.foreign-numeric-vector :force t)
;;  (asdf:oos 'asdf:compile-op 'org.middleangle.cl-blapack :force t)

;;; The maing thing...
;; (asdf:oos 'asdf:compile-op 'lisp-matrix :force t)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

(run-lisp-matrix-tests)  ;; 56 4 2 ;; 16.10.2008
(describe  (run-lisp-matrix-tests))

(in-package :lisp-matrix-user)

;; (lisp-matrix-unittests:run-lisp-matrix-tests)
;; (describe (lisp-matrix-unittests:run-lisp-matrix-tests))


;; Here is what we need to fix, based on the above:
;; #  make-predicate (and variable-capture, see macro-expansion)
;; #  col / row  on transposed matrices
;; #  mref access, first index boundaries (second index is controlled,
;;    and errors thrown as needed
;; #  

(progn ;; THESE WORK!

  (progn ;; SETUP DATA
    ;; make some matrices

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
    
    (defvar m2-la nil
      "placeholder 2")
    ;; would an API similar to:
    ;;    :initial-contents (:row-major (list 1 2 3 4 5 6 7 8 9 0))
    ;; be useful?
    (setf m2-la (make-matrix 2 5
			  :implementation :lisp-array  ;; :foreign-array
			  :element-type 'integer ; 'double-float
			  ;; :initial-contents (list 1 2 3 4 5 6 7 8 9 10)
			  :initial-contents #2A(( 1 2 3 4 5)
						( 6 7 8 9 10))))

    ;; Currently we can make a foriegn matrix of doubles, but not a
    ;; foriegn matrix of integers.
    (defvar m2-fa nil
      "placeholder 2")
    (setf m2-fa (make-matrix 2 5
			   :implementation :foreign-array 
			   :element-type 'double-float
			   :initial-contents #2A(( 1d0 2d0 3d0 4d0 5d0)
						 ( 6d0 7d0 8d0 9d0 10d0))))



    ;; extract a view of m01, but just rows 1 and 3.
    (strides m01 :nrows 2 :row-stride 2)

    )  ; EVAL HERE TO SETUP DATA

  ;;;;;;; FIX ALL THE ERRORS

  ;; FIXME: need to get the foriegn-friendly arrays package involved
  ;; to create integer matrices.  Or do we just throw an error that
  ;; says to use lisp-arrays?
  (defvar m2a nil
    "placeholder 2")
  (setf m2a (make-matrix 2 5
			:implementation :foreign-array 
			:element-type 'integer 
			:initial-contents #2A(( 1 2 3 4 5)
					      ( 6 7 8 9 10))))

  ;; FIXME -- bad error!!
  ;; This index isn't correct, and it doesn't barf correctly. 
  m01
  (mref m01 1 8) ; good -- we throw an error... but
  (mref m01 8 1) ; BAD! barfs, not protecting against first index...
  (setf (mref m01 7 7) 1.2d0)
  m1
  ;; Reason -- possibly related to the storage forward, i.e. lisp-
  ;; vs. foreign- centric arrays.


  (m* m01 (transpose m01))
  ;; FIXME: the following has no applicable method -- only for
  ;; doubles, not integers.  
  (m* m2 (transpose m2))


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

  (col m4 1)
  (col (transpose m4) 1) ;; this is the problem, indexing issue...


  ;; and the same problem.
  m3 
  (transpose m3)
  (v= (col m3 1) (row (transpose m3) 1))
  (v= (row m3 1) (col (transpose m3) 1))

  ;; striding examples
#|
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 2 :row-stride 2)))
    (ensure (m= (col b 0)
		(make-matrix 2 1 :initial-contents '((1d0) (11d0)))))
    (ensure (m= (col b 1)
		(make-matrix 2 1 :initial-contents '((2d0) (12d0)))))
    (ensure (m= (col b 2)
		(make-matrix 2 1 :initial-contents '((3d0) (13d0)))))
    (ensure (m= (col b 3)
		(make-matrix 2 1 :initial-contents '((4d0) (14d0)))))
    (ensure (m= (col b 4)
		(make-matrix 2 1 :initial-contents '((5d0) (15d0))))))
|#
  ;; examples of striding -- need more!
  m3
  (strides m3 :nrows 2 :row-stride 2) ;; skip a row
  (strides m3 :nrows 3) ;; first 3 rows
  (strides m3 :ncols 3 :col-stride 2) ;; cols 1, 3 ,5
  (strides m3 :ncols 2) ;; first 2 cols

  m3
  ;; (slice m3 ...) -- strides provide sections of matrix; slicing provides vectors.


  ;; slicing isn't affected by transposition -- doesn't affect the
  ;; counting through.   Should this be the case?  (need to migrate to unit-tests
  (v=  (slice m3 :offset 5 :stride  2 :nelts 3 :type :row)
       (slice (transpose m3) :offset 5 :stride  2 :nelts 3 :type :row))
  (v=  (slice m3 :offset 5 :stride  2 :nelts 3 :type :row)
       (slice (transpose m3) :offset 5 :stride  2 :nelts 3 :type :column))
  ;; and note the above -- vector equality doesn't depend on orientation...

  (slice m3 :offset 1 :stride  2 :nelts 3 :type :column)
  (slice m3 :offset 1 :stride  0 :nelts 3 :type :column)
  ;; :type   : provides the form to provide output for
  ;; :offset : number of observations (in "col/row major"
  ;;           matrix-dependent order) to skip over before starting
  ;;           extraction
  ;; :stride : 0 = repeat same value; 1, as ordered, 2 every other, 
  ;;           etc... 
  )


;;; EXAMPLES TO DEMONSTRATE

;; convert between foriegn-array and lisp-array.

;; operate ()

;; do some blas/lapack

;; output

