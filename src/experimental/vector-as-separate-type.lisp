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
;;;; row- and column-vectors.
;;;; 
;;;; This is a try at defining vectors as a separate type.

;;;; ** Basic vector class

(defclass vector-like ()
  ((nelts :initarg :nelts
          :reader nelts
          :initform 0
          :documentation "Number of elements in this vector or vector
          view."))
  (:documentation "Abstract base class for 1-D vectors and vector
  views."))

(defgeneric vector-dimension (vector)
  (:documentation "Like ARRAY-DIMENSION for vector-like objects.")
  (:method ((vector vector-like))
    (nelts vector)))

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

(defmethod ancestor ((vector vector-like)) vector)

;;;; ** Vector views (VECVIEW)

(defclass vecview (vector-like)
  ((parent :initarg :parent
           :reader parent
           :documentation "The \"parent\" object to which this vector
           view relates."))
  (:documentation "An abstract class representing a \"view\" into a
  vector.  That view may be treated as a (readable and writeable)
  reference to the elements of the vector."))

(defgeneric vecview-p (vector)
  (:documentation "Is VECTOR a VECVIEW?"))

(defmethod ancestor ((vector vecview)) (ancestor (parent vector)))

(defgeneric real-nelts (vector)
  (:documentation "Return the actual number of elements of the vector
  in which VECTOR is stored, namely the number of columns of the
  ancestor of VECTOR.")
  (:method ((vector vector-like)) (nelts vector))
  (:method ((vector vecview)) (nelts (ancestor vector))))

(defclass slice-vecview (vecview)
  ((offset :initarg :offset
           :reader offset
           :initform 0)
   (stride :initarg :stride
           :reader stride
           :initform 1)))

(defmethod unit-stride-p ((vector vector-like))
  t)

(defmethod unit-stride-p ((vector vecview))
  (unit-stride-p (parent vector)))

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

(defgeneric make-vector* (nelts implementation &key element-type
                                initial-element)
  (:documentation "Create a vector holding NELTS elements of type
  ELEMENT-TYPE with IMPLEMENTATION as underlying implementation.
  INITIAL-ELEMENT is an element that may be used to initially fill the
  vector.

  If INITIAL-ELEMENT is not specified, the vector is not initialized,
  and accessing its elements will thus return spurious values."))

(defun make-vector (nelts &key
                    (implementation *default-implementation*)
                    (element-type *default-element-type*)
                    (initial-element nil initial-element-p)
                    (initial-contents nil initial-contents-p))
  "Create a vector holding NELTS elements of type ELEMENT-TYPE with
  VECTOR-IMPLEMENTATION as underlying implementation.  INITIAL-ELEMENT
  is an element that may be used to initially fill the vector.

  If INITIAL-ELEMENT is not specified, the vector is not initialized,
  and accessing its elements will thus return spurious values.

  If INITIAL-CONTENTS is specified, it is used to initialize the
  vector, by using the generic function COPY!.

  IMPLEMENTATION can be one of :LISP-ARRAY and :FOREIGN-ARRAY"
  (when (and initial-element-p initial-contents-p)
    (error "Both INITIAL-ELEMENT and INITIAL-CONTENTS should not be ~
    specified"))
  (let ((vector (apply #'make-vector* nelts implementation
                       :element-type element-type
                       (when initial-element-p
                         (list :initial-element initial-element)))))
    (when initial-contents
      (copy! initial-contents vector))
    vector))

(defmethod implementation ((vector vecview))
  (implementation (parent vector)))

(defmethod element-type ((vector vecview))
  (element-type (parent vector)))

;;;; *** Vector views

(defgeneric slice-class (vector)
  (:documentation "Return the name of the class to be used for a slice
  of VECTOR.")
  (:method ((vector vector-like)) 'slice-vecview))

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

;;;; *** Specific vectors

(defun vones (nelts &key
              (implementation *default-implementation*)
              (element-type *default-element-type*))
  (make-vector nelts :implementation implementation
               :element-type element-type
               :initial-element (coerce 1 element-type)))

(defun vzeros (nelts &key
               (implementation *default-implementation*)
               (element-type *default-element-type*))
  (make-vector nelts :implementation implementation
               :element-type element-type
               :initial-element (coerce 0 element-type)))

(defun vrand (nelts &key
              (implementation *default-implementation*)
              (element-type *default-element-type*)
              (state *random-state*))
  ;; FIXME: doesn't work for complex types
  (check-type state random-state)
  (let ((vector (make-vector nelts :implementation implementation
                                   :element-type element-type))
        (one (coerce 1 element-type)))
    (dotimes (i nelts)
      (setf (vref vector i) (random one state)))
    vector))

;;;; ** Vector operations

;;;; *** Copying

(defmethod copy! ((a vector-like) (b vector-like))
  (assert (= (nelts a) (nelts b)))
  (assert (subtypep (element-type a) (element-type b)))
  (unless (eq a b)
    (dotimes (i (nelts a))
      (setf (vref b i) (vref a i))))
  b)

(defmethod copy! ((a array) (b vector-like))
  (unless (and (= (array-rank a) 1)
               (= (array-dimension a 0) (nelts b)))
    (error "A doesn't have the correct dimensions"))
  (let ((element-type (element-type b)))
    (dotimes (i (nelts b))
      (assert (typep (aref a i) element-type))
      (setf (vref b i) (aref a i))))
  b)

(defmethod copy! ((a list) (b vector-like))
  (unless (= (nelts b) (length a))
    (error "A doesn't have the correct dimensions"))
  (let ((element-type (element-type b)))
    (loop for i below (nelts b) for elt in a do
          (assert (typep elt element-type))
          (setf (vref b i) elt)))
  b)

(defmethod copy* ((vector vector-like) implementation)
  (make-vector (nelts vector)
               :implementation implementation
               :element-type (element-type vector)
               :initial-contents vector))
