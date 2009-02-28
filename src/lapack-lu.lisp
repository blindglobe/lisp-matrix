

;;; LU

;;; GETRF - compute the LU Factorization of a matrix.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method getrf ((a !matrix-type))
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
    (unless ipvt ; make it the bigger of # cols/ # rows
      (setf ipvt (make-fnv-int32 (nrows a) :initial-value 0)))
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p) 
		     t))
      (list a
	    (check-info (fnv-int32-ref info 0) "GETRF"))
      (call-with-work  (lwork work !data-type)
		       (!function (nrows a)        ; N 
				  (ncols a)        ; M
				  a                ; A
				  (max 1 (ncols a)); LDA
				  ipiv
;;        IPIV    (output) INTEGER array, dimension (min(M,N))
;;                The  pivot  indices;  for 1 <= i <= min(M,N), row i of
;; 		  the matrix was interchanged with row IPIV(i).
				  info))))); info
