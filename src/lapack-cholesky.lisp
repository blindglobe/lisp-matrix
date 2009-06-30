(in-package :lisp-matrix)

;;; CHOLESKY

;;; POTRF - compute the Cholesky Factorization of a real sym pos-def
;;; matrix A.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method potrf ((a !matrix-type))
  (assert (<= (ncols a) (nrows a))) ; LAPACK condition, kill in lisp
				    ; not fortran.
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)))
      (list a
	    "U"
	    (check-info (fnv-int32-ref info 0) "POTRF"))
      (!function "U"            ; store in Upper Triang.  Option for "L"?
		 (ncols a)      ; N 
		 a              ; matrix (in/out)
		 (real-nrows a) ; LDA
		 info))))       ; info

#|

DPOTRF(3)                                                                        DPOTRF(3)

NAME   DPOTRF  -  compute  the Cholesky factorization of a real symmetric positive definite
       matrix A

SYNOPSIS
       SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )

           CHARACTER      UPLO
           INTEGER        INFO, LDA, N
           DOUBLE         PRECISION A( LDA, * )

PURPOSE
       DPOTRF computes the Cholesky factorization of a  real  symmetric  positive  definite
       matrix A.  The factorization has the form
          A = U**T * U,  if UPLO = ’U’, or
          A = L  * L**T,  if UPLO = ’L’,
       where U is an upper triangular matrix and L is lower triangular.

       This is the block version of the algorithm, calling Level 3 BLAS.

ARGUMENTS
       UPLO    (input) CHARACTER*1
               = ’U’:  Upper triangle of A is stored;
               = ’L’:  Lower triangle of A is stored.
       N       (input) INTEGER
               The order of the matrix A.  N >= 0.
       A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
               On  entry,  the symmetric matrix A.  If UPLO = ’U’, the leading N-by-N upper
               triangular part of A contains the upper triangular part of the matrix A, and
               the  strictly  lower triangular part of A is not referenced.  If UPLO = ’L’,
               the leading N-by-N lower triangular part of A contains the lower  triangular
               part  of  the  matrix  A, and the strictly upper triangular part of A is not
               referenced.
               On exit, if INFO = 0, the factor U or L from the Cholesky factorization A  =
               U**T*U or A = L*L**T.
       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,N).
       INFO    (output) INTEGER
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an illegal value
               >  0:   if  INFO = i, the leading minor of order i is not positive definite,
               and the factorization could not be completed.

LAPACK version 3.0                      15 June 2000                              DPOTRF(3)

|#




;;; CHOLESKY
;;
;; POTRI - compute the inverse of a real symmetric positive definite
;; matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
(def-lapack-method potri ((a !matrix-type))
  (assert (= (ncols a) (nrows a))) ; only square matrices
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)
		     t))
	;; Returning:
	;; - inverse,
	;; - "U" since upper format trangular,
	;; - info, for correctness of results.  
	;; Should we put INFO first?!
	(list a
	      "U" ; not useful until we add option for lowercase.
	      (check-info (fnv-int32-ref info 0) "POTRI"))
      (!function "U"        ; "L" (in) is lower an option? 
		 (ncols a)  ; N (in) (order of matrix, columns, 2nd index
		 a          ; a (in/out) matrix
		 (nrows a)  ; LDA (in) leading dimension, LDA >= max(1,N)
		            ; above was "(real-nrows a)" ?
		 info))))   ; info (out)


(def-lapack-method potrs ((a !matrix-type) (b !matrix-type) ipiv-a)
  (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
  (let ((uplo "U")
	(info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p))
		  (b (or (not unit-strides-p)
                         transposed-p)))
      (list b
	    (check-info (fnv-int32-ref info 0) "POTRS"))
      (!function uplo      ; matrix orientation, None, Transpose, Adjoint
		 (ncols a) ; N
		 (ncols b) ; NHRS
		 a	   ; A
		 (nrows a) ; LDA
		 b         ; B
		 (nrows b) ; LDB
		 info)))) ; info

#|
NAME   DPOTRS  - solve a system of linear equations A*X = B with a symmetric positive defi‐
       nite matrix A using the Cholesky factorization A = U**T*U or A = L*L**T computed  by
       DPOTRF

SYNOPSIS
       SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )

           CHARACTER      UPLO
           INTEGER        INFO, LDA, LDB, N, NRHS
           DOUBLE         PRECISION A( LDA, * ), B( LDB, * )

PURPOSE
       DPOTRS  solves  a system of linear equations A*X = B with a symmetric positive defi‐
       nite matrix A using the Cholesky factorization A = U**T*U or A = L*L**T computed  by
       DPOTRF.

ARGUMENTS
       UPLO    (input) CHARACTER*1
               = ’U’:  Upper triangle of A is stored;
               = ’L’:  Lower triangle of A is stored.
       N       (input) INTEGER
               The order of the matrix A.  N >= 0.
       NRHS    (input) INTEGER
               The number of right hand sides, i.e., the number of columns of the matrix B.
               NRHS >= 0.
       A       (input) DOUBLE PRECISION array, dimension (LDA,N)
               The triangular factor U or L from the Cholesky factorization A = U**T*U or A
               = L*L**T, as computed by DPOTRF.
       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,N).
       B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
               On entry, the right hand side matrix B.  On exit, the solution matrix X.
       LDB     (input) INTEGER
               The leading dimension of the array B.  LDB >= max(1,N).
       INFO    (output) INTEGER
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an illegal value
|#

(defun minv-cholesky (a)
  "invert A using LU Factorization.  A must be symmetric."
  (check-type a matrix-like)
  (assert (matrix-like-symmetric-p a))
  (assert  (= (nrows a) (ncols a)))
  (let ((a-fac (first  (potrf (copy a)))))
    (trap2mat  (first (potri a-fac)))))

#+nil(progn
	(let* ((m1 (rand 3 3))
	       (m1tm1 (m* (transpose m1) m1)))
	  (m* m1tm1 (minv-cholesky m1tm1))))

(defun msolve-cholesky (a b)
  "Compute `x1' solving `A x = b', with LU factorization."
  (let ((a-fac (potrf (copy a))))
    (first (potrs (first a-fac) b (second a-fac)))))

#+nil (progn
	(let* ((a (rand 3 3))
	       (x-pre (rand 3 1))
 	       (b (m* a x-pre)))
	  (m- x-pre (msolve-cholesky a b))))
