;;; need license and etc data.

;;; This file contains lisp-centric matrix manipulations which aren't
;;; necessarily sophisticated numerical linear algebra.  However, it
;;; relies on BLAS/LAPACK for reasonably numerical ops, and lisp for
;;; "standard" non-numerical ops such as bind (should we overload
;;; concatenate instead?)

;;; access to substructures should be found in [matrix|vector].lisp. 

(in-package :lisp-matrix)

(defmacro with-typed-values ((&rest bindings) matrix &body body)
  "Each binding in BINDINGS is of the form (VARIABLE VALUE).  VARIABLE
  is bound to VALUE coerced to the element type of MATRIX."
  (with-unique-names (element-type)
    (labels ((make-coerced-binding (binding)
               (destructuring-bind (variable value) binding
                 `(,variable (coerce ,value ,element-type)))))
     `(let ((,element-type (element-type ,matrix)))
        (let (,@(mapcar #'make-coerced-binding bindings))
          ,@body)))))

(defgeneric m* (a b)
  (:documentation "Matrix multiplication: A * B.   Defaults to the
  element type of the first matrix.

  Better approach would be to consider lowest-common-type?")
  (:method ((a matrix-like) (b matrix-like))
    (assert (= (ncols a) (nrows b))) ;; insist on squareness...
    (with-typed-values ((one 1)
                        (zero 0)) a
      (let ((c (make-matrix (nrows a) (ncols b)
                            :element-type (element-type a))))
        (gemm one a b zero c)))))

(defgeneric m+ (a b)
  (:documentation "Matrix addition: A + B.")
  (:method ((a matrix-like) (b matrix-like))
    (with-typed-values ((one 1)) a
      (axpy one a (copy b)))))

(defgeneric m- (a b)
  (:documentation "Matrix subtraction: A - B.")
  (:method ((a matrix-like) (b matrix-like))
    (with-typed-values ((minus-one -1)) a
      (axpy minus-one b (copy a))))
#|;; must handle more types!  Somehow we are missing something... 
  (:method ((mata matrix-like) (matb matrix-like))
    (assert (and (equal (matrix-dimensions mata)
			(matrix-dimensions matb))))
    (let ((result (make-matrix (values-list (matrix-dimensions mata)))))
      (dotimes (i (matrix-dimension mata 0))
	(dotimes (j (matrix-dimension mata 1))
	  (setf (mref result i j) (- (mref mata i j) (mref matb i j)))))
      result)))
|#
  )

;; TODO: SUM is not yet done
#+ (or)
(defgeneric sum (matrix)
  (:documentation "")
  (:method ((matrix matrix-like))
    (asum matrix)))


;;; We need to consider the equivalent of rbind/cbind operations, for
;;; building larger matrices from smaller but "dimension-matching"
;;; matrices. 


;; next variant, bind, should handle "unlimited" arguments to bind
;; together.  There might be a destructuring bind,
;; i.e. metabang-bind-style, where we manage to specify the structure
;; and how to remove, and given a proposed structure, how to put in. 
(defgeneric bind2 (m1 m2 &key by)
  (:documentation "Simple experiment, not necessarily part of the API
  yet!  When type is :row, If the number of columns of m1 and m2
  match, join them.  Think of a sandwich approach, resulting in:

         m1
         --
         m2 

  The ARGS can be matrices, vectors, or lists. Arguments are bound
  into a matrix along their rows. Example:

    (bind2 #2a((1 2)(3 4)) #(5 6) :by :row) 
  returns
    #2a((1 2)(3 4)(5 6))

  When type is :column, if the number of rows of m1 and m2 match, join 
  them.  Think of a pair of columns, resulting in 
 
         m1 | m2 

  API should result with the ARGS as matrices, vectors, or
  lists. Arguments are bound into a matrix along their columns.
  Example: 
    (bind2 #2a((1 2)(3 4)) #(5 6) :by :column) 
  returns 
    #2a((1 2 5)(3 4 6))"))

(defmethod bind2 ((m1 matrix-like) (m2 matrix-like) &key (by :row))
  "Binding for matrix, columns, deep copy into a new matrix of the
  right size.   Could we solve the row-binding approach by transpose?" 
  (ecase by
    (:column ;; mostly right
     (progn
       (assert (= (nrows m1) (nrows m2)))
       (let* ((nr (nrows m1))
	      (nc (+ (ncols m1) (ncols m2)))
	      (mincol (min (ncols m1) (ncols m2)))
	      (addcol (- (max (ncols m1) (ncols m2))
			 (min (ncols m1) (ncols m2))))
	      (m (make-matrix nr nc)))
	 (dotimes (i nr)
	   (dotimes (j mincol) ; copy equal parts 
	     (setf (mref m i j) (mref m1 i j))
	     (setf (mref m i (+ j (ncols m1))) (mref m2 i j)))
	   (if (> (ncols m1) (ncols m2)) ; copy the excess part
	       (dotimes (j addcol) (setf (mref m i (+ j mincol))
					 (mref m1 i (+ j mincol))))
	       (dotimes (j addcol) (setf (mref m i (+ j mincol (ncols m1)))
	 				 (mref m2 i (+ j mincol ))))))
	 m)))
    (:row ;; mostly wrong
     (progn 
       (assert (= (ncols m1) (ncols m2)))
       (let* ((nr (+ (nrows m1) (nrows m2)))
	      (nc (ncols m1))
	      (minrow (min (nrows m1) (nrows m2)))
	      (addrow (- (max (nrows m1) (nrows m2))
			 (min (nrows m1) (nrows m2))))
	      (m (make-matrix nr nc)))
	 (dotimes (j nc)
	   (dotimes (i minrow) ; copy equal parts 
	     (setf (mref m i j) (mref m1 i j))
	     (setf (mref m (+ i (nrows m1)) j) (mref m2 i j)))
	   (if (> (nrows m1) (nrows m2)) ; copy the excess part
	       (dotimes (i addrow) (setf (mref m (+ i minrow) j)
					 (mref m1 (+ i minrow) j)))
	       (dotimes (i addrow) (setf (mref m (+ i minrow (nrows m1)) j)
					 (mref m2 (+ i minrow) j)))))
	 m)))
    (t (error "Problems"))))




;;; also on the list would be outer-product, but that should come from
;;; LAPACK?

(defgeneric cross-product (mata matb))

#|

(defgeneric outer-product (mata matb &optional op)
  (:documentation "compute outer product of 2 arrays.")
  (:method ((mata t) (matb t) &optional (op t))
    (error
     "Outer Product not implemented for objects of type ~S and ~S"
     mata matb))
  (:method ((mata matrix-like)
	    (matb matrix-like)
	    &optional
	    (op t))

    (let* ((resultdims (list (xdims 1 mata)
			     (xdims 2 mata)
			     (xdims 1 matb)
			     (xdims 2 matb)))
	   (mresult (make-array resultdims)))
      (loop 
	 over i j k l in resultdims
	   (setf (xref mresult i j k l)
		 (funcall op
			  (xref mata i j) (xref matb j k))))
      mresult)))
|#





;;; Element-wide operations.  API is similar to matlisp

(defgeneric m.+ (mata matb)
  (:documentation "same as m+ which is inherently an element-wise operation.")
  (:method ((mata matrix-like) (matb matrix-like)) (m+ mata matb)))


(defgeneric m.- (mata matb)
  (:documentation "same as m- which is inherently an element-wise operation.")
  (:method ((mata matrix-like) (matb matrix-like)) (m- mata matb)))


(defgeneric m.* (mata matb)
  (:documentation "same as m+ which is inherently an element-wise
  operation. How should we handle coercion?  probably the right way to
  do this will be to consider the least specific form, and coerce
  back.  HOWEVER, this could be done simpler by barfing (forcing
  explicit coercion) and this would be safer, numerically.")
  (:method ((mata matrix-like) (matb matrix-like))
    (assert (and (equal (matrix-dimensions mata)
			(matrix-dimensions matb))))
    (let ((result (make-matrix (matrix-dimension mata 0)
			       (matrix-dimension mata 1))))
      (dotimes (i (matrix-dimension mata 0))
	(dotimes (j (matrix-dimension mata 1))
	  (setf (mref result i j) (* (mref mata i j) (mref matb i j)))))
      result)))



(defgeneric m./ (mata matb)
  (:documentation "same as m+ which is inherently an element-wise
  operation. How should we handle coercion?  probably the right way to
  do this will be to consider the least specific form, and coerce
  back.  HOWEVER, this could be done simpler by barfing (forcing
  explicit coercion) and this would be safer, numerically.")
  (:method ((mata matrix-like) (matb matrix-like))
    (assert (and (equal (matrix-dimensions mata)
			(matrix-dimensions matb))))
    (let ((result (make-matrix (matrix-dimension mata 0)
			       (matrix-dimension mata 1))))
      (dotimes (i (matrix-dimension mata 0))
	(dotimes (j (matrix-dimension mata 1))
	  (setf (mref result i j) (/ (mref mata i j) (mref matb i j)))))
      result)))

;;; Need equiv of R's apply or the googly python's map-reduce

(defun list-of-rows (M)
  "Returns a list of vector-like elements from matrix M.
FIXME: AWFUL IMPLEMENTATION"
  (let ((result nil))
    (dotimes (i (nrows M))
      (setf result (append result (list  (row M i)) )))
    result))
;;(list-of-rows m01)

(defun list-of-columns (M)
  "Returns a list of vector-like elements from matrix M.
FIXME: AWFUL."
  (let ((result nil))
    (dotimes (i (ncols M))
      (setf result (append result (list (col M i)))))
    result))
#|
  ;; Is this right?
  (defun list-of-margins (M margin-type)) ;; 
  (defun list-of-matrix-partitions (M partition-walker)) ; could be
					; generalized to return diff
					; types.
  (defun list-of-vector-partitions (M partition-walker))
|#

;; (defgeneric map-matrix (withfn mat &key iterator result-type)
;;   (:documentation "equivalent of R's apply commands.  But with a sense
;;   of extensibility.")
;;   (:method (withfn (mat 'vector-like) &key iterator result-type)
;;     (let ((result))
;;       )))

;;; Need to add a walker in the sense of the affi accessor approach,
;;; but needs to have a reset,


;;; LAPACK-related numerical linear algebra.  Should we be precise?

#|
 (defmethod qr-decomp (a)
  (:documentation "Compute the QR decomposition of matrix A")
  (:method ( (a fa-matrix-double))
    (let ((tau (make-fnv-int32 (min (nrows a) (ncols a)) :initial-value 0)))
      (dgeqrf a tau)
      (values a tau))))
|#
