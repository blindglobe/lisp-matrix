(in-package :lisp-matrix)


(define-abstract-class vector-like ()
  (:documentation "Abstract base class for vectors and vector views 
                   whose elements are stored in a foreign-numeric-vector.")
  ((nelts :initarg :nelts 
	  :initform 0 
	  :reader :nelts
	  :documentation "Number of elements in this vector (view)")))

(defmethod vector-dimension ((x vector-like))
  (nelts x))

(defmethod initialize-instance :after ((x vector-like) &key)
  "Make sure that the vector-like object has a valid (non-negative) 
   number of elements."
  (if (< (nelts x) 0)
      (error "VECTOR-LIKE objects cannot have a negative number ~A of elements." 
	     (nelts x))
      (call-next-method)))

(define-abstract-class vecview (vector-like)
  (:documentation "An abstract class representing a \"view\" into a vector.
                   That view may be treated as a (readable and writeable)
                   reference to the elements of the matrix.")
  ((parent :initarg :parent
	   :reader :parent
	   :documentation "The \"parent\" object to which this vector view relates.")))





(eval-when (:compile-toplevel :load-toplevel)

  (defmacro make-typed-vector (fnv-type)
    "Template constructor macro for VECTOR and related object types that
     hold data using FNV objects of type FNV-TYPE (which is the FNV datatype
     suffix, such as complex-double, float, double, etc.)."
    (let ((fnv-ref (ncat fnv-type '-ref))
	  (make-fnv (ncat 'make-fnv- fnv-type))
	  (vector-type-name (ncat 'vector- fnv-type))
	  (vector-slice-type-name (ncat 'vector-slice- fnv-type)))

      `(progn
	 (defclass ,vector-type-name (vector-like)
	   (:documentation ,(format nil "Dense vector holding elements of type ~A" fnv-type))
	   ((data :initarg :data
		  ;; No INITFORM provided because users aren't supposed to
		  ;; initialize this object directly, only by calling the 
		  ;; appropriate "generic" factory function.
		  :documentation "The FNV object holding the elements."
		  :reader :data)))

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

	 (defmethod fnv-type-to-vector-type ((type (eql ,fnv-type))
					     vector-category)
	   (cond ((eq matrix-type :vector)
		  ,vector-type-name)
		 ((eq matrix-type :slice)
		  ,vector-slice-type-name)
		 (t 
		  (error "Invalid vector type ~A" vector-type))))

	 (defmethod vector-type-to-fnv-type ((type (eql ,vector-type-name)))
	   ,fnv-type)
	 (defmethod vector-type-to-fnv-type ((type (eql ,vector-slice-type-name)))
	   ,fnv-type)
	 (defmethod fnv-type ((x ,vector-type-name))
	   ,fnv-type)
	 (defmethod fnv-type ((x ,vector-slice-type-name))
	   ,fnv-type)))))

;;; Instantiate the classes and methods.
(make-typed-vector 'double)
(make-typed-vector 'float)
(make-typed-vector 'complex-double)
(make-typed-vector 'complex-float)


(defmethod slice ((x vector-like) 
		  &key (offset 0) (stride 1) (nelts (nelts x)))
  "Returns a \"slice\" (readable and writeable reference to a potentially
   strided range of elements) of the given vector-like object x."
  (make-instance (fnv-type-to-vector-type (fnv-type x) :slice)
		 :parent parent
		 :nelts nelts
		 :offset offset
		 :stride stride))


(defun make-vector (nelts 
		    &key fnv-type
		    (initial-element 0 initial-element-provided-p)
		    (initial-contents nil initial-contents-provided-p))

  "\"Generic\" method for creating a vector, given the number of elements
   NELTS, and optionally either an initial element INITIAL-ELEMENT or the 
   initial contents INITIAL-CONTENTS (a 1-D array with dimension NELTS), 
   which are deep-copied into the resulting vector."

  ;; Logic for handling initial contents or initial element.
  ;; Initial contents take precedence (if they are specified,
  ;; then any supplied initial-element argument is ignored).
  
  (let ((data (if initial-contents-provided-p
		  (let ((fnv (funcall (symbol-function (ncat 'make-fnv- 
							     fnv-type)) nelts)))
		    (labels ((fnv-ref (x i)
			       (funcall (symbol-function (ncat 'fnv- fnv-type 
							       '-ref)) 
					x i)))
		      (dotimes (i nelts fnv)
			(setf (fnv-ref fnv i) (aref initial-contents i)))))
		  (if initial-element-provided-p
		      (funcall (symbol-function (ncat 'make-fnv- fnv-type)) 
			       (* nrows ncols) 
			       :initial-element initial-element)
		      (funcall (symbol-function (ncat 'make-fnv- fnv-type))
			       (* nrows ncols))))))
    (make-instance (fnv-type-to-matrix-type fnv-type :vector)
		   :nrows nrows
		   :ncols ncols
		   :data data)))

