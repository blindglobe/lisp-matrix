(in-package :lisp-matrix)

;;;; * Vectors
;;;;
;;;; Vector can be viewed as matrices that happen to have one row (or
;;;; one column), or as a separate type.  We will define vectors as
;;;; matrices that have a restriction: one row or one column.
;;;;
;;;; One advantage of having vectors be subtypes of matrices is that
;;;; we don't need to re-specialize many generic functions (e.g., m*,
;;;; m+, m-, etc.), we can just use those that are defined for
;;;; matrices.
;;;;
;;;; A disadvantage is that we will have some code duplication for the
;;;; case of row- and column-vectors.

(defclass vector-like (matrix-like) ()
  (:documentation "Abstract base class for 1-D vectors and vector
  views."))

(defgeneric vref (vector i)
  (:documentation "Return the I-th element of VECTOR.  This method is
  slow as it requires CLOS method dispatch and index calculation(s),
  and should thus be replaced with vectorized or block operations
  whenever possible"))

(defgeneric (setf vref) (value vector i)
  (:documentation "Set the I-th element of VECTOR to VALUE.  This
  method is slow as it requires CLOS method dispatch and index
  calculation(s), and should thus be replaced with vectorized or block
  operations whenever possible."))

(defclass row-vector (vector-like)
  ((nrows :allocation :class
          :initform 1)))

(defmethod vref ((vector row-vector) i)
  (mref vector 0 i))

(defmethod (setf vref) (value (vector row-vector) i)
  (setf (mref vector 0 i) value))

(defclass col-vector (vector-like)
  ((ncols :allocation :class
          :initform 1)))

(defmethod vref ((vector col-vector) i)
  (mref vector i 0))

(defmethod (setf vref) (value (vector col-vector) i)
  (setf (mref vector i 0) value))

(defclass vecview (vector-like)
  ((parent :initarg :parent
           :reader parent)))

(defclass transpose-vecview (vecview transpose-matview) ())

(defmethod vref ((vector transpose-vecview) i)
  (vref (parent vector) i))

(defmethod (setf vref) (value (vector transpose-vecview) i)
  (setf (vref (parent vector) i) value))

(defclass slice-vecview (vecview)
  ((offset :initarg :offset
           :reader offset
           :initform 0)
   (stride :initarg :stride
           :reader stride
           :initform 1)))

(defclass row-slice-vecview (slice row-vector) ())
(defclass col-slice-vecview (slice row-vector) ())

(defmethod vref ((vector slice-vecview) i)
  (vref (parent vector)
        (+ (offset vector) (* i (stride vector)))))

(defmethod (setf vref) (value (vector slice-vecview) i)
  (setf (vref (parent vector)
              (+ (offset vector) (* i (stride vector))))
        value))

(defgeneric slice-class (vector)
  (:documentation "Return the name of the class to be used for a slice
  of VECTOR.")
  (:method ((vector row-vector)) 'row-slice-vecview)
  (:method ((vector col-vector)) 'col-slice-vecview))

(defgeneric slice (vector &key offset stride nelts)
  (:documentation "Create a slice view of VECTOR.")
  (:method (vector &key (offset 0) (stride 1) (nelts (nelts vector)))
    (make-instance (slice-class vector)
                   :parent vector
                   :nelts nelts
                   :offset offset
                   :stride stride)))

(defmethod transpose-class ((matrix vector-like)) 'transpose-vecview)
