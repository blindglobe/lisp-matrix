(in-package :lisp-matrix)

;;;; * Vectors
;;;;
;;;; Vector can be viewed as matrices that happen to have one row (or
;;;; one column), or as a separate type.
;;;;
;;;; With a separate type, would it make sense to define row vectors
;;;; and column vectors?
;;;;
;;;; One advantage of having vectors be subtypes of matrices is that
;;;; we don't need to re-specialize many generic functions (e.g., m*,
;;;; m+, m-, etc.), we can just use those that are defined for
;;;; matrices.
;;;;
;;;; But when we need to know that we have a vector, we can use that
;;;; type information..
;;;;
;;;; Anyway, if we have both row vectors and column vectors, we can
;;;; easily switch between the two by using the function TRANSPOSE,
;;;; without making a view.

(defclass vector-like (matrix-like) ())

(defclass row-vector (vector-like)
  ((nrows :allocation :class
          :initform 1)))

(defclass col-vector (vector-like)
  ((ncols :allocation :class
          :initform 1)))

(defclass vecview (vector-like)
  ((parent :initarg :parent
           :reader parent)))

(defgeneric vref (vector i)
  (:method ((vector row-vector) i)
    (mref vector 0 i))
  (:method ((vector col-vector) i)
    (mref vector i 0)))

(defclass slice (vecview)
  ((offset :initarg :offset
           :reader offset
           :initform 0)
   (stride :initarg :stride
           :reader stride
           :initform 1)))

(defclass row-slice (slice row-vector) ())
(defclass col-slice (slice row-vector) ())

(defmethod vref ((vector slice) i)
  (vref (parent vector)
        (+ (offset vector) (* i (stride vector)))))

(defgeneric slice-class (vector)
  (:method ((vector row-vector)) 'row-slice)
  (:method ((vector col-vector)) 'col-slice))

(defun slice (vector &key (offset 0) (stride 1)
              (nelts (nelts vector)))
  (make-instance (slice-class vector)
                 :parent vector
                 :nelts nelts
                 :offset offset
                 :stride stride))
