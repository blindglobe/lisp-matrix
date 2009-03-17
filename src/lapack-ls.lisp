(in-package :lisp-matrix)

;;; Least Squares (in the generalized sense) solvers.
;;; by that, including LS, WLS, and GLS.

;;; Solving ax = b
;;; note that 'a' will be modified upon the call, and will have a
;;; different value at the end, more appropriate to the transformation
;;; (i.e. will be the QR, but stored in common compact form).

(def-lapack-method (gelsy :function-names
			  ((%sgelsy single-float single-float)
			   (%dgelsy double-float double-float)))
    ((a !matrix-type)
     (b !matrix-type)
     rcond &optional jpvt)
  ;; FIXME: has both LWORK and RWORK for %ZGELSY and %CGELSY
  ;; so need to handle those via explicit methods
  (unless jpvt
    (setq jpvt (make-fnv-int32 (ncols a) :initial-value 0)))
  (let ((rank (make-fnv-int32 1 :initial-value 0))
        (info (make-fnv-int32 1 :initial-value 0)))
    ;; FIXME: B needs to be resized anyway if A has more columns than
    ;; rows, to allow for enough storage for the result matrix =>
    ;; quick fix: disallow this
    (assert (<= (ncols a) (nrows a)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p))
                  (b (or (not unit-strides-p)
                         transposed-p)
		     t))
        ;; FIXME: the value RANK is not correct because the cffi type
        ;; :FORTRAN-INT does not define a TRANSLATE-FROM-FOREIGN method?
        ;; => why not use a standard cffi integer type anyway??
        ;; (a fix is to make RANK a fnv-int32 with one element
        (progn
          (check-info (fnv-int32-ref info 0) "GELSY")
          (list (if (= (nrows b) (ncols a)) ; returns (list b rank)
                    b
                    (window b :nrows (ncols a)))
                (fnv-int32-ref rank 0)))
      (call-with-work (lwork work !data-type)
		      (!function (nrows a) ; M
				 (ncols a) ; N
				 (ncols b) ; NRHS
				 a ; A 
				 (real-nrows a) ; LDA
				 b ; B 
				 (real-nrows b) ; LDB
				 jpvt ; JPVT
				 rcond ; RCOND
				 rank ; RANK
				 (data work) ; WORK
				 lwork ; LWORK
				 info))))) ; INFO

#|
NAME   DGELSY  -  compute  the  minimum-norm solution to a real linear
       least squares problem

SYNOPSIS
       SUBROUTINE DGELSY( M, N, NRHS, A, LDA,  B,  LDB,  JPVT,  RCOND,
                          RANK, WORK, LWORK, INFO )

           INTEGER        INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
           DOUBLE         PRECISION RCOND
           INTEGER        JPVT( * )
           DOUBLE         PRECISION  A( LDA, * ), B( LDB, *), WORK(*)
PURPOSE

       DGELSY computes the minimum-norm solution to a real linear
       least squares problem:

             minimize || A * X - B ||

       using a complete orthogonal factorization of A.  A is an M-by-N
       matrix which may be rank-deficient.

       Several right hand side vectors b and solution vectors x can be
       handled in a single call; they are stored as the columns of the
       M-by-NRHS right hand side matrix B and the N-by-NRHS solution
       matrix X.

       The routine first computes a QR factorization with column
       pivoting:

           A * P = Q * [ R11 R12 ]
                       [  0  R22 ]

       with R11 defined as the largest leading submatrix whose
       estimated condition number is less than 1/RCOND.  The order of
       R11, RANK, is the effective rank of A.

       Then, R22 is considered to be negligible, and R12 is
       annihilated by orthogonal transformations from the right,
       arriving at the complete orthogonal factorization:

          A * P = Q * [ T11 0 ] * Z
                      [  0  0 ]
       The minimum-norm solution is then

          X = P * Z’ [ inv(T11)*Q1’*B ]
                     [        0       ]
       where Q1 consists of the first RANK columns of Q.

       This routine is basically identical to the original xGELSX
       except three differences:

         o The call to the subroutine xGEQPF has been substituted by
           the the call to the subroutine xGEQP3.  This subroutine is
           a Blas-3 version of the QR factorization with column
           pivoting.
         o Matrix B (the right hand side) is updated with Blas-3.
         o The permutation of matrix B (the right hand side) is faster
           and more simple.

ARGUMENTS
       M       (input) INTEGER
               The number of rows of the matrix A.  M >= 0.

       N       (input) INTEGER
               The number of columns of the matrix A.  N >= 0.

       NRHS    (input) INTEGER
               The number of right hand sides, i.e., the number of
               columns of matrices B and X. NRHS >= 0.

       A       (input/output)   DOUBLE  PRECISION  array,  dimension
               (LDA,N) On entry, the M-by-N matrix A.  On exit, A has
               been overwritten by details of its complete orthogonal
               factorization.

       LDA     (input) INTEGER
               The leading dimension of the array A.  LDA >= max(1,M).

       B       (input/output) DOUBLE PRECISION array,
               dimension (LDB,NRHS) On entry, the M-by-NRHS right hand
               side matrix B.  On exit, the N-by-NRHS solution matrix
               X.

       LDB     (input) INTEGER
               The   leading   dimension   of  the  array  B.  LDB  >=
               max(1,M,N).

       JPVT    (input/output) INTEGER array, dimension (N)
               On entry, if JPVT(i) .ne. 0, the i-th column  of  A  is
               permuted  to  the  front of AP, otherwise column i is a
               free column.  On exit, if JPVT(i) = k,  then  the  i-th
               column of AP was the k-th column of A.

       RCOND   (input) DOUBLE PRECISION
               RCOND  is  used  to  determine the effective rank of A,
               which is defined as the order of  the  largest  leading
               triangular  submatrix  R11 in the QR factorization with
               pivoting of  A,  whose  estimated  condition  number  <
               1/RCOND.

       RANK    (output) INTEGER
               The  effective rank of A, i.e., the order of the subma‐
               trix R11.  This is the same as the order of the  subma‐
               trix T11 in the complete orthogonal factorization of A.

       WORK    (workspace/output) DOUBLE  PRECISION  array,  dimension
       (LWORK)
               On exit, if INFO  =  0,  WORK(1)  returns  the  optimal
               LWORK.

       LWORK   (input) INTEGER
               The  dimension of the array WORK.  The unblocked strat‐
               egy requires that: LWORK >= MAX( MN+3*N+1, 2*MN+NRHS ),
               where  MN  = min( M, N ).  The block algorithm requires
               that: LWORK >= MAX(  MN+2*N+NB*(N+1),  2*MN+NB*NRHS  ),
               where NB is an upper bound on the blocksize returned by
               ILAENV for the routines DGEQP3, DTZRZF, STZRQF, DORMQR,
               and DORMRZ.

               If  LWORK  = -1, then a workspace query is assumed; the
               routine only calculates the optimal size  of  the  WORK
               array,  returns  this  value  as the first entry of the
               WORK array, and no error message related  to  LWORK  is
               issued by XERBLA.

       INFO    (output) INTEGER
               = 0: successful exit
               <  0:  If  INFO  = -i, the i-th argument had an illegal
               value.

FURTHER DETAILS
       Based on contributions by
         A.  Petitet,  Computer  Science  Dept.,   Univ.   of   Tenn.,
       Knoxville, USA
         E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I,
       Spain
         G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I,
       Spain

LAPACK version 3.0           15 June 2000                    DGELSY(3)
|#

#+nil
(progn
  (let ((*default-implementation* :foreign-array))
    (let* ((m 10)
	   (n 10)
	   (a (rand m n))
	   (x (rand n 1))
	   (b (m* a x))
	   (rcond (* (coerce (expt 2 -52) 'double-float)
		     (max (nrows a) (ncols a))))
	   (orig-a (copy a))
	   (orig-b (copy b))
	   (orig-x (copy x)))
      (list x (gelsy a b rcond)))))

#+nil
(setf *temp-result* 
      (let ((*default-implementation* :lisp-array))
	(let* ((m 10)
	       (n 10)
	       (a (rand m n))
	       (x (rand n 1))
	       (b (m* a x))
	       (rcond (* (coerce (expt 2 -52) 'double-float)
			 (max (nrows a) (ncols a))))
	       (orig-a (copy a))
	       (orig-b (copy b))
	       (orig-x (copy x)))
	  (list x (gelsy a b rcond)))))



#+nil
(progn
  ;; consider Y = X b (or normal approach, X b = Y)
  (defparameter *gelsy-result*
    (let* ((n 10)
	   (p 5)
	   (x-temp (rand n p))
	   (b-temp (rand p 1))
	   (y-temp (m* x-temp b-temp))  ;; so Y=Xb
	   (rcond (* (coerce (expt 2 -52) 'double-float)
		     (max (nrows x-temp) (ncols y-temp)))))
      ;; should be numerically 0
      (v- b-temp (first  (gelsy x-temp y-temp rcond)))))

  (princ  *gelsy-result*) )


(defun least-squares-gelsy (x y)
  "Solves:
       X beta = Y,
for beta."
  (list x (gelsy x y
		 (* (coerce (expt 2 -52) 'double-float)
		    (max (nrows x) (ncols x))))))
