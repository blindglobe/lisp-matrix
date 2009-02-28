(in-package :lisp-matrix)

;;; CHOLESKY

;;; POTRF - compute the Cholesky Factorization of a real sym pos-def
;;; matrix A.
;;; Returns Matrix, upper/lower triang char, info
(def-lapack-method potrf ((a !matrix-type))
  (let ((info (make-fnv-int32 1 :initial-value 0)))
    (assert (<= (ncols a) (nrows a))) ; make sure A supports options 
    (with-copies ((a (or (not unit-strides-p)
                         transposed-p) 
		     t))
	(list a
	      "U"
	      (check-info (fnv-int32-ref info 0) "POTRF"))
      (!function "U"            ; store in Upper section
		 (ncols a)      ; N 
		 a              ; matrix (in/out)
		 (real-nrows a) ; LDA
		 info))))       ; info

;;; POTRI - compute the inverse of a real symmetric positive definite
;;; matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
(def-lapack-method potri ((a !matrix-type))
  (assert (= (ncols a) (nrows a)))  ;; only works with square matrices
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


