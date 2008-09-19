(in-package :lisp-matrix)


(define-abstract-class vector-like ()
  ((nelts :initarg :nelts 
	  :initform 0 
	  :reader nelts
	  :documentation "Number of elements in this vector (view)"))
  (:documentation "Abstract base class for vectors and vector views 
                   whose elements are stored in a foreign-numeric-vector."))

(defgeneric vector-dimension (x)
  (:documentation "Like ARRAY-DIMENSION for vector-like objects.")
  (:method ((x vector-like))
    (nelts x)))

(defmethod initialize-instance :after ((x vector-like) &key)
  "Make sure that the vector-like object has a valid (non-negative) 
   number of elements."
  (if (< (nelts x) 0)
      (error "VECTOR-LIKE objects cannot have a negative number ~A of elements." 
	     (nelts x))))

(define-abstract-class vecview (vector-like)
  ((parent :initarg :parent
	   :reader parent
	   :documentation "The \"parent\" object to which this vector
	   view relates."))
  (:documentation "An abstract class representing a \"view\" into a vector.
                   That view may be treated as a (readable and writeable)
                   reference to the elements of the matrix."))

(defgeneric make-vector (nelts fnv-type &key initial-element
                               initial-contents)
  (:documentation "Generic method for creating a vector, given the
  number of elements NELTS, and optionally either an initial element
  INITIAL-ELEMENT or the initial contents INITIAL-CONTENTS (a 1-D
  array with dimension NELTS), which are deep-copied into the
  resulting vector."))

(defgeneric vref (a i)
  (:documentation "Like AREF buf for vectors."))

;;; Internal generic functions

(defgeneric fnv-type-to-vector-type (type vector-category)
  (:documentation "Given a particular FNV type (such as
  'complex-float) and a keyword indicating the kind of vector (:VECTOR
  or :SLICE), returns the corresponding specific vector type."))

(defgeneric vector-type-to-fnv-type (type)
  (:documentation "Return the FNV type (such as 'COMPLEX-FLOAT)
  corresponding to a given type of vector (such as
  'VECTOR-COMPLEX-FLOAT or 'VECTOR-SLICE-COMPLEX-FLOAT)."))

;;; Macro to make the actual matrix classes

(eval-when (:compile-toplevel :load-toplevel)

  (defmacro make-typed-vector (fnv-type)
    "Template constructor macro for VECTOR and related object types that
     hold data using FNV objects of type FNV-TYPE (which is the FNV datatype
     suffix, such as complex-double, float, double, etc.)."
    (let ((fnv-ref (make-symbol* "FNV-" fnv-type "-REF"))
	  (make-fnv (make-symbol* "MAKE-FNV-" fnv-type))
	  (vector-type-name (make-symbol* "VECTOR-" fnv-type))
	  (vector-slice-type-name (make-symbol* "VECTOR-SLICE-" fnv-type)))

      `(progn
	 (defclass ,vector-type-name (vector-like)
	   ((data :initarg :data
		  ;; No INITFORM provided because users aren't supposed to
		  ;; initialize this object directly, only by calling the 
		  ;; appropriate "generic" factory function.
		  :documentation "The FNV object holding the elements."
		  :reader data))
	   (:documentation ,(format nil "Dense vector holding elements of type ~A" fnv-type)))

	 (defmethod vref ((A ,vector-type-name) i)
	   (declare (type fixnum i))
	   (,fnv-ref (data A) i))
	 
	 ;; TODO: set up SETF to work with VREF.
	 
	 (defclass ,vector-slice-type-name (vecview)
	   ((offset :initarg :offset
		     :initform 0
		     :reader offset)
	    (stride :initarg :stride
		    :initform 1
		    :reader stride)))

	 (defmethod vref ((A ,vector-slice-type-name) i)
	   (declare (type fixnum i))
	   (with-slots (parent offset stride) A
	     (vref parent (+ offset (* i stride)))))

	 (defmethod fnv-type-to-vector-type ((type (eql ',fnv-type))
					     vector-category)
	   (cond ((eq vector-category :vector)
		  ',vector-type-name)
		 ((eq vector-category :slice)
		  ',vector-slice-type-name)
		 (t 
		  (error "Invalid vector type ~A" vector-category))))

	 (defmethod vector-type-to-fnv-type
             ((type (eql ',vector-type-name)))
	   ',fnv-type)
	 (defmethod vector-type-to-fnv-type
             ((type (eql ',vector-slice-type-name)))
	   ',fnv-type)
	 (defmethod fnv-type ((x ,vector-type-name))
	   ',fnv-type)
	 (defmethod fnv-type ((x ,vector-slice-type-name))
	   ',fnv-type)

         (defmethod make-vector (nelts (fnv-type (eql ',fnv-type)) &key
                                 (initial-element 0)
                                 (initial-contents nil initial-contents-p))
           ;; Logic for handling initial contents or initial element.
           ;; Initial contents take precedence (if they are specified,
           ;; then any supplied initial-element argument is ignored).
           (let ((data
                  (cond
                    (initial-contents-p
                     (let ((fnv (,make-fnv nelts)))
                       (dotimes (i nelts fnv)
                         ;; FIXME: INITIAL-CONTENTS may be a list as
                         ;; in MAKE-ARRAY -- Evan Monroig 2008.04.24
                         (setf (,fnv-ref fnv i)
                               (aref initial-contents i)))))
                    (t
                     (,make-fnv nelts 
                                :initial-value initial-element)))))
             (make-instance ',vector-type-name
                            :nelts nelts
                            :data data)))))))

;;; Instantiate the classes and methods.
(make-typed-vector double)
(make-typed-vector float)
(make-typed-vector complex-double)
(make-typed-vector complex-float)

(defgeneric slice (x &key offset stride nelts)
  (:documentation "Returns a \"slice\" (readable and writeable
  reference to a potentially strided range of elements) of the given
  vector-like object x.")
  (:method ((x vector-like) 
            &key (offset 0) (stride 1) (nelts (nelts x)))
    (make-instance (fnv-type-to-vector-type (fnv-type x) :slice)
                   :parent x
                   :nelts nelts
                   :offset offset
                   :stride stride)))

