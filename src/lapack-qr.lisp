(in-package :lisp-matrix)

;;; QR decomposition.  

;;; Need one more front end to provide appropriate processing.  A and
;;; TAU will have different values at the end, more appropriate to the
;;; transformation (i.e. will be the QR, but stored in common compact
;;; form).

;;        M       (input) INTEGER
;;                The number of rows of the matrix A.  M >= 0.
;;        N       (input) INTEGER
;;                The number of columns of the matrix A.  N >= 0.
;;        A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
;;                On entry, the M-by-N matrix A.  On exit, the  elements  on  and
;;                above the diagonal of the array contain the min(M,N)-by-N upper
;;                trapezoidal matrix R (R is upper triangular if  m  >=  n);  the
;;                elements  below the diagonal, with the array TAU, represent the
;;                orthogonal matrix Q as a product of min(m,n) elementary reflec‐
;;                tors (see Further Details).
;;        LDA     (input) INTEGER
;;                The leading dimension of the array A.  LDA >= max(1,M).
;;        TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
;;                The  scalar  factors  of the elementary reflectors (see Further
;;                Details).
;;        WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
;;                On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
;;        LWORK   (input) INTEGER
;;                The dimension of the array WORK.  LWORK >= max(1,N).  For opti‐
;;                mum  performance  LWORK >= N*NB, where NB is the optimal block‐
;;                size.
;;                If LWORK = -1, then a workspace query is assumed;  the  routine
;;                only  calculates  the  optimal  size of the WORK array, returns
;;                this value as the first entry of the WORK array, and  no  error
;;                message related to LWORK is issued by XERBLA.
;;        INFO    (output) INTEGER
;;                = 0:  successful exit
;;                < 0:  if INFO = -i, the i-th argument had an illegal value

(def-lapack-method geqrf ((a !matrix-type)
			  ;; (tau !matrix-type) ; tau is for output
			  )
  (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
  (let ((info (make-fnv-int32 1 :initial-value 0))
	(tau (make-matrix (nrows a) (ncols a)
			  :element-type (element-type a))))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p))
                  (tau (or (not unit-strides-p)
			   transposed-p)))
	(list a
	      tau
	      (check-info (fnv-int32-ref info 0) "GEQRF"))
      (call-with-work (lwork work !data-type)
		      (!function (nrows a)
				 (ncols a)
				 a
				 (max 1 (nrows a))
				 (data tau)
				 (data work)
				 lwork
				 info)))))
