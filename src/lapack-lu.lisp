(in-package :lisp-matrix)

;;; LU-decomp tools

;;; Need: tests

;;; GETRF - compute the LU Factorization of a matrix.
;;; Returns Matrix, upper/lower triang char, info
#|

DGETRF(3)                                    )                                    DGETRF(3)

NAME   DGETRF - compute an LU factorization of a general M-by-N matrix A using partial piv‐
       oting with row interchanges

SYNOPSIS
       SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
           INTEGER        INFO, LDA, M, N
           INTEGER        IPIV( * )
           DOUBLE         PRECISION A( LDA, * )

PURPOSE
       DGETRF computes an LU factorization of a general M-by-N matrix A using partial  piv‐
       oting with row interchanges.  The factorization has the form
          A = P * L * U
       where  P  is a permutation matrix, L is lower triangular with unit diagonal elements
       (lower trapezoidal if m > n), and U is upper triangular (upper trapezoidal  if  m  <
       n).

       This is the right-looking Level 3 BLAS version of the algorithm.

ARGUMENTS
       M       (input) INTEGER
               The number of rows of the matrix A.  M >= 0.

       N       (input) INTEGER
               The number of columns of the matrix A.  N >= 0.

       A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
               On  entry,  the  M-by-N matrix to be factored.  On exit, the factors L and U
               from the factorization A = P*L*U; the unit diagonal elements of  L  are  not
               stored.

       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,M).

       IPIV    (output) INTEGER array, dimension (min(M,N))
               The  pivot  indices;  for 1 <= i <= min(M,N), row i of the matrix was inter‐
               changed with row IPIV(i).

       INFO    (output) INTEGER
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an illegal value
               > 0:  if INFO = i, U(i,i) is exactly zero. The factorization has  been  com‐
               pleted,  but  the  factor  U  is exactly singular, and division by zero will
               occur if it is used to solve a system of equations.

LAPACK version 3.0                      15 June 2000                              DGETRF(3)
|#

(def-lapack-method getrf ((a !matrix-type) &optional ipiv)
  (assert (<= (ncols a) (nrows a))) ; A must be NxM, N>=M 
  (let ((info (make-fnv-int32 1 :initial-value 0))
	(ipiv-local (if ipiv
			ipiv
			;; make it the bigger of # cols/ # rows
			(make-fnv-int32 (max (nrows a) (ncols a))
					:initial-value 0))))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p)))
	(list a ; compact PLU form.
	    ipiv-local
	    (check-info (fnv-int32-ref info 0) "GETRF"))
      (!function (nrows a)        ; M in 
		 (ncols a)        ; N in 
		 a                ; A in/out
		 (max 1 (ncols a)); LDA in
		 ;; IPIV (output) INTEGER array, dimension (min(M,N))
		 ;;   The pivot indices; for 1 <= i <= min(M,N), row i of
		 ;;   the matrix was interchanged with row IPIV(i)
		 ipiv-local ; OUT
		 info)))); info

;;;;;;;;;;;;

;;; GETRI - invert a matrix using LU factorization.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method getri ((a !matrix-type) ipiv)
  (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
  (let ((info (make-fnv-int32 1 :initial-value 0))
	(ipiv-local (if ipiv
			ipiv
			;; make it the bigger of # cols/ # rows
			(make-fnv-int32 (max (nrows a) (ncols a))
					:initial-value 0))))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p) 
		     t))
      (list a
	    ipiv-local
	    (check-info (fnv-int32-ref info 0) "GETRF"))


      (call-with-work (lwork work !data-type)
		      (!function (ncols a)        ; M
				 a                ; A
				 (nrows a)		   ; N 
				 ipiv ; from getrf result
				 work  ; array for comp
				 lwork ; dimension of work
				 info))))); info

#|
DGETRI(3)                              )                             DGETRI(3)

NAME   DGETRI  -  compute  the  inverse of a matrix using the LU factorization
       computed by DGETRF

SYNOPSIS
       SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )

           INTEGER        INFO, LDA, LWORK, N
           INTEGER        IPIV( * )
           DOUBLE         PRECISION A( LDA, * ), WORK( * )

PURPOSE
       DGETRI computes the inverse of a matrix using the LU factorization com‐
       puted  by  DGETRF.   This  method inverts U and then computes inv(A) by
       solving the system inv(A)*L = inv(U) for inv(A).

ARGUMENTS
       N       (input) INTEGER
               The order of the matrix A.  N >= 0.

       A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
               On entry, the factors L and U from the factorization A =  P*L*U
               as  computed  by  DGETRF.  On exit, if INFO = 0, the inverse of
               the original matrix A.

       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,N).

       IPIV    (input) INTEGER array, dimension (N)
               The pivot indices from DGETRF; for 1<=i<=N, row i of the matrix
               was interchanged with row IPIV(i).

       WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
               On exit, if INFO=0, then WORK(1) returns the optimal LWORK.

       LWORK   (input) INTEGER
               The dimension of the array WORK.  LWORK >= max(1,N).  For opti‐
               mal performance LWORK >= N*NB, where NB is the  optimal  block‐
               size returned by ILAENV.

               If  LWORK  = -1, then a workspace query is assumed; the routine
               only calculates the optimal size of  the  WORK  array,  returns
               this  value  as the first entry of the WORK array, and no error
               message related to LWORK is issued by XERBLA.

       INFO    (output) INTEGER
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an illegal value
               > 0:  if INFO = i, U(i,i) is exactly zero; the matrix is singu‐
               lar and its inverse could not be computed.

LAPACK version 3.0               15 June 2000                        DGETRI(3)
|#




;;; GETRS - Solve Ax=b using LU factorization.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method getrs ((a !matrix-type) (b !matrix-type) ipiv-a)
  (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
  (let ((trans "N")
	(info (make-fnv-int32 1 :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p))
		  (b (or (not unit-strides-p)
                         transposed-p)))
      (list b
	    (check-info (fnv-int32-ref info 0) "GETRS"))
      (!function trans     ; matrix orientation, None, Transpose, Adjoint
		 (ncols a) ; N
		 (ncols b) ; NHRS
		 a	   ; A
		 (nrows a) ; LDA
		 ipiv-a    ; from getrf result
		 b         ; B
		 (nrows b) ; LDB
		 info)))) ; info
#|
DGETRS(3)                                    )                                    DGETRS(3)

NAME   DGETRS  -  solve a system of linear equations A * X = B or A’ * X = B with a general
       N-by-N matrix A using the LU factorization computed by DGETRF

SYNOPSIS
       SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )

           CHARACTER      TRANS
           INTEGER        INFO, LDA, LDB, N, NRHS
           INTEGER        IPIV( * )
           DOUBLE         PRECISION A( LDA, * ), B( LDB, * )

PURPOSE
       DGETRS solves a system of linear equations A * X = B or A’ * X = B with a general N-
       by-N matrix A using the LU factorization computed by DGETRF.

ARGUMENTS
       TRANS   (input) CHARACTER*1
               Specifies the form of the system of equations:
               = ’N’:  A * X = B  (No transpose)
               = ’T’:  A’* X = B  (Transpose)
               = ’C’:  A’* X = B  (Conjugate transpose = Transpose)
       N       (input) INTEGER
               The order of the matrix A.  N >= 0.
       NRHS    (input) INTEGER
               The number of right hand sides, i.e., the number of columns of the matrix B.
               NRHS >= 0.
       A       (input) DOUBLE PRECISION array, dimension (LDA,N)
               The factors L and U from the factorization A = P*L*U as computed by  DGETRF.
       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,N).
       IPIV    (input) INTEGER array, dimension (N)
               The  pivot  indices from DGETRF; for 1<=i<=N, row i of the matrix was inter‐
               changed with row IPIV(i).
       B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
               On entry, the right hand side matrix B.  On exit, the solution matrix X.
       LDB     (input) INTEGER
               The leading dimension of the array B.  LDB >= max(1,N).
       INFO    (output) INTEGER
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an illegal value

LAPACK version 3.0                      15 June 2000                              DGETRS(3)

|#



;;; LU common applications

(defun minv-lu (a)
  "invert A using LU Factorization"
  (let ((a-fac (getrf (copy a))))
    (first (getri (first a-fac) (second a-fac)))))

#+nil (progn
	(let ((m1 (rand 3 3)))
	  (m* m1 (minv-lu m1))))

(defun msolve-lu (a b)
  "Compute `x1' solving `A x = b', with LU factorization."
  (let ((a-fac (getrf (copy a))))
    (first (getrs (first a-fac) b (second a-fac)))))

#+nil (progn
	(let* ((a (rand 3 3))
	       (x-pre (rand 3 1))
 	       (b (m* a x-pre)))
	  (m- x-pre (msolve-lu a b))))
