(in-package :lisp-matrix)

;;;; * Vectors
;;;;
;;;; Vector can be viewed as matrices that happen to have one row (or
;;;; one column), or as a separate type.
;;;;
;;;; One advantage of having vectors be subtypes of matrices is that
;;;; we don't need to re-specialize many generic functions (e.g., m*,
;;;; m+, m-, etc.), we can just use those that are defined for
;;;; matrices.
;;;;
;;;; However, a big disadvantage is that we will have lots of code
;;;; duplication (two times as many classes) for the separation of
;;;; row vectors and column vectors.
;;;; 
;;;; This is a try at defining vectors as a subtype of matrices, but
;;;; without class duplication for row vectors and column vectors.  We
;;;; simply add a generic function to determine if the vector is a
;;;; row-vector or a column vector.

;;;; ** Basic vector class

(defclass vector-like (matrix-like)
  ()
  (:documentation "Abstract base class for 1-D vectors and vector
  views."))

(defgeneric vector-dimension (vector)
  (:documentation "Like ARRAY-DIMENSION for vector-like objects.")
  (:method ((vector vector-like))
    (nelts vector)))

(defgeneric vector-type (vector)
  (:documentation "Whether the vector is considered as a row
  vector (:ROW) or a column vector (:COLUMN) for matrix operations.
  This has no effect on storage since the values are stored
  contiguously, or with a stride for vector views.")
  (:method ((vector vector-like))
    (if (col-vector-p vector) :column :row)))

(defgeneric col-vector-p (matrix)
  (:method ((matrix matrix-like))
    "A general matrix cannot be a column vector.  We will arrange for
    functions building matrices to return column vectors when
    appropriate."
    nil)
  (:method ((matrix vector-like))
    (= 1 (ncols matrix))))

(defgeneric row-vector-p (matrix)
  (:method ((matrix matrix-like))
    "A general matrix cannot be a row vector.  We will arrange for
    functions building matrices to return row vectors when
    appropriate."
    nil)
  (:method ((matrix vector-like))
    (= 1 (nrows matrix))))

(defgeneric check-invariant (vector)
  (:documentation "Check the class invariant of VECTOR, namely that it
  has one column or one row.")
  (:method ((vector vector-like))
    (or (col-vector-p vector) (row-vector-p vector))))

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

;;;; ** Vector views (VECVIEW)

(defclass vecview (vector-like matview)
  ((parent :initarg :parent
           :reader parent
           :type matrix-like            ; <-- not a typo
           :documentation "The \"parent\" object to which this vector
           view relates."))
  (:documentation "An abstract class representing a \"view\" into a
  vector.  That view may be treated as a (readable and writeable)
  reference to the elements of the vector."))

(defgeneric vecview-p (vector)
  (:documentation "Is VECTOR a VECVIEW?")
  (:method (vector) nil)
  (:method ((vector vecview)) t))

(defgeneric real-nelts (matrix)
  (:documentation "Return the actual number of elements of the vector
  in which MATRIX is stored, namely the number of columns of the
  ancestor of MATRIX.")
  (:method ((matrix matrix-like)) (nelts matrix))
  (:method ((matrix matview)) (nelts (ancestor matrix))))

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

(defmethod unit-stride-p ((vector slice-vecview))
  (and (= 1 (stride vector))
       (unit-stride-p (parent vector))))

(defmethod vref ((vector slice-vecview) i)
  (vref (parent vector)
        (+ (offset vector) (* i (stride vector)))))

(defmethod (setf vref) (value (vector slice-vecview) i)
  (setf (vref (parent vector)
              (+ (offset vector) (* i (stride vector))))
        value))

;;;; ** Creating vectors
;;;;
;;;; Vectors are automatically created by matrix creation methods when
;;;; one of the dimensions is 1 (one), or

;;;; *** Vector views

(defmethod transpose-class ((matrix vector-like)) 'transpose-vecview)

(defgeneric slice-class (matrix)
  (:documentation "Return the name of the class to be used for a slice
  of MATRIX.")
  (:method ((matrix matrix-like)) 'slice-vecview))

#+(or)
(defgeneric slice (vector &key offset stride nelts)
  (:documentation "Create a slice view of VECTOR.")
  (:method ((vector vector-like)
            &key (offset 0) (stride 1) (nelts (nelts vector)))
    (make-instance (slice-class vector)
                   :parent vector
                   :nelts nelts
                   :offset offset
                   :stride stride))
  (:method ((vector slice-vecview)
            &key (offset 0) (stride 1) (nelts (nelts vector)))
    "For a slice on a slice-vecview, we can directly compute the slice
    parameters based on the parent of VECTOR."
    (make-instance (slice-class vector)
                   :parent (parent vector)
                   :nelts nelts
                   :offset (+ offset (offset vector))
                   :stride (* stride (stride vector)))))

(defgeneric slice (matrix &key offset stride nelts type)
  (:documentation "Create a slice view of MATRIX.")
  (:method (matrix &key (offset 0) (stride 1) (nelts (nelts matrix))
            (type :row))
    (make-instance (slice-class matrix)
                   :parent matrix
                   :nrows (ecase type (:row 1) (:column nelts))
                   :ncols (ecase type (:row nelts) (:column 1))
                   :offset offset
                   :stride stride)))

;; FIXME: not finished
(defgeneric row (matrix i)
  (:documentation "Return a view on a given row of MATRIX.")
  (:method ((matrix matrix-like) (i integer))
    (assert (< -1 i (nrows matrix)))
    ;; depending on the orientation of the matrix,
    (ecase (orientation matrix)
      (:column (slice matrix
                      :offset (flatten-matrix-indices matrix i 0)
                      :stride (nrows matrix)
                      :nelts (ncols matrix)
                      :type :row))
      (:row (slice matrix
                   :offset (flatten-matrix-indices matrix i 0)
                   :stride 1
                   :nelts (ncols matrix)
                   :type :row)))))

#||

(defparameter *a* (rand 3 4 :element-type 'single-float))

(defmethod print-object ((object vector-like) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(vref (row *a* 0) 0)

||#
