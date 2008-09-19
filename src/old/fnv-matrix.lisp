(in-package :lisp-matrix)

(defun valid-orientation-p (x)
  (or (eq x :column) (eq x :row)))

(defun opposite-orientation (x)
  (cond ((eq x :column) :row)
	((eq x :row) :column)
	(t (error "Invalid orientation ~A" x))))

(define-abstract-class matrix-like ()
  ((nrows :initarg :nrows :initform 0 :accessor nrows
	  :documentation "Number of rows in the matrix (view)")
   (ncols :initarg :ncols :initform 0 :accessor ncols
	  :documentation "Number of columns in the matrix (view)")
   ;; Later we'll add different storage formats, such as :general, :upper, :lower, (:banded lbw ubw), but we have some design issues to work out first.
   )
  (:documentation
   "Abstract base class for 2-D matrices and matrix views.  We assume
   for now that matrices are stored in column order (Fortran style),
   for BLAS and LAPACK compatibility."))

(defmethod initialize-instance :after ((A matrix-like) &key)
  "Error checking for initialization of a MATRIX-LIKE object."
  (with-slots (nrows ncols) A
    (assert (>= nrows 0))
    (assert (>= ncols 0))))

(defgeneric nelts (x)
  (:documentation "Default method for computing the number of elements
  of a matrix.  For obvious reasons, this will be overridden for
  subclasses that implement sparse matrices.")
  (:method ((x matrix-like))
    (* (nrows x) (ncols x))))

(defgeneric matrix-dimension (a which)
  (:documentation "Like ARRAY-DIMENSION for matrix-like objects.")
  (:method ((A matrix-like) which)
    (cond ((= which 0) (nrows A))
          ((= which 1) (ncols A))
          (t (error "The given matrix has only two dimensions, but you tried to access dimension ~A"
                    (1+ which))))))

(defgeneric matrix-dimensions (a)
  (:documentation "Like ARRAY-DIMENSIONS for matrix-like objects.")
  (:method ((A matrix-like)) 
    (list (nrows A) (ncols A))))

(define-abstract-class matview (matrix-like) 
  ((parent :initarg :parent
	   :reader parent
	   :documentation "The \"parent\" object to which this matrix
	   view relates."))
  (:documentation "An abstract class representing a \"view\" into a matrix.
                   That view may be treated as a (readable and writeable) 
                   reference to the elements of the matrix."))

(define-abstract-class window-matview (matview)
  ((offset0 :initarg :offset0
            :initform 0
            :reader offset0)
   (offset1 :initarg :offset1
            :initform 0
            :reader offset1))
  (:documentation "A WINDOW-MATVIEW views a block of elements in the 
              underlying matrix that is conceptually 2-D contiguous.  If the 
              underlying matrix is column-oriented, the elements in each column 
              of a WINDOW-MATVIEW are stored contiguously, and horizontally 
              adjacent elements are separated by a constant stride (\"LDA\" in 
              BLAS terms)."))

(define-abstract-class transpose-matview (matview)
  ()
  (:documentation "A TRANSPOSE-MATVIEW views the transpose of a
  matrix.  If you want a deep copy, call the appropriate COPY function
  on the transpose view.  The reason for this is to avoid expensive
  operations whenever possible.  Many BLAS and LAPACK routines have a
  \"TRANSA\" argument (or similar) that lets you specify that the
  operation should work with the transpose of the matrix.  This means
  that it usually isn't necessary to compute an explicit transpose, at
  least for input arguments."))

(define-abstract-class strided-matview (window-matview)  
  ((stride0 :initarg :stride0
            :initform 1
            :reader stride0
            :documentation "Stride in the row direction")
   (stride1 :initarg :stride1
            :initform 1
            :reader stride1
            :documentation "Stride in the column direction"))
  (:documentation "A STRIDED-MATVIEW views a window of the matrix with
  a particular stride in each direction (the stride can be different
  in each direction)."))

(defmethod parent ((matrix matrix-like)) nil)

(defgeneric make-matrix (nrows ncols fnv-type &key initial-element
                               initial-contents)
  (:documentation "Generic method for creating a matrix, given the
  number of rows NROWS, the number of columns NCOLS, and optionally
  either an initial element INITIAL-ELEMENT or the initial contents
  INITIAL-CONTENTS (a 2-D array with dimensions NROWS x NCOLS), which
  are (deep) copied into the resulting matrix."))

(defgeneric flatten-matrix-indices (a i j)
  (:documentation "Given an index pair (I,J) into the given matrix A,
  returns the 1-D index corresponding to the location in the
  underlying storage in which the element A(i,j) is stored."))

(defgeneric mref (a i j)
  (:documentation "(MREF A i j) gives you the (i,j)-th element of the
  matrix A. This method is slow (as it requires CLOS method dispatch
  and index calculation(s)) and is thus to be replaced with vectorized
  or block operations whenever possible."))

(defgeneric orientation (a)
  (:documentation "lisp-matrix objects are stored in column order."))

;;; internal generic functions

(defgeneric fnv-type-to-matrix-type (type matrix-type)
  (:documentation "Given a particular FNV type (such as
  'complex-float) and a keyword indicating the kind of matrix or
  matrix view, returns the corresponding specific matrix (view)
  type."))

(defgeneric matrix-type-to-fnv-type (type)
  (:documentation "Return the FNV type (such as 'COMPLEX-FLOAT)
  corresponding to a given type of matrix (such as
  'MATRIX-COMPLEX-FLOAT or 'WINDOW-MATVIEW-COMPLEX-FLOAT or
  'TRANSPOSE-MATVIEW-COMPLEX-FLOAT or
  STRIDED-MATVIEW-COMPLEX-FLOAT)."))

(defgeneric unit-stride-p (a)
  (:documentation "Tests for \"unit stride.\" (The strided matrix view
  is the only view which causes itself and its children possibly not
  to have unit stride.)"))

(defgeneric fnv-type (a)
  (:documentation "Given an object of a particular matrix (view) type,
  returns the corresponding FNV type (such as 'double, 'complex-float,
  etc.)."))

(defgeneric transposed-p (matrix)
  (:documentation "Is MATRIX a transposed view of its ancestor
  matrix?"))

(defmethod transposed-p ((matrix matrix-like))
  (and (parent matrix)
       (transposed-p (parent matrix))))

(defmethod transposed-p ((matrix transpose-matview))
  t)

(defgeneric zero-offset-p (matrix)
  (:documentation "Has MATRIX a zero offset (as for window and stride
  views)?"))

(defmethod zero-offset-p ((matrix matrix-like))
  (if (parent matrix)
      (zero-offset-p (parent matrix))
      t))

(defmethod zero-offset-p ((matrix window-matview))
  (and (= 0 (offset0 matrix) (offset1 matrix))
       (zero-offset-p (parent matrix))))

(defgeneric copy-into (a b)
  (:documentation "Copy A into B if they are not the same object, and
  return B.  A and B should have the same dimensions."))

(defmethod copy-into ((a matrix-like) (b matrix-like))
  (assert (= (ncols a) (ncols b)))
  (assert (= (nrows a) (nrows b)))
  ;; FIXME: care about fast copy once everything is working
  (unless (eq a b)
    (dotimes (i (nrows a))
      (dotimes (j (ncols a))
        (setf (mref b i j) (mref a i j)))))
  b)

(defgeneric copy (a)
  (:documentation "Return a deep copy of a matrix A."))

(defmethod copy ((a matrix-like))
  (copy-into a (make-matrix (nrows a) (ncols a) (fnv-type a))))

(defgeneric copy-maybe (a test)
  (:documentation "Return a deep copy of a matrix A if TEST is
  satisfied, or return A itself."))

(defmethod copy-maybe ((a matrix-like) test)
  (if (funcall test a)
      (copy a)
      a))

(defgeneric real-nrows (a)
  (:documentation "Return the actual number of rows of the matrix into
  which A is stored, i.e. the number of rows of the ancestor of A.")
  (:method ((a matrix-like))
    (if (parent a)
        (real-nrows (parent a))
        (nrows a))))

(defgeneric real-ncols (a)
  (:documentation "Return the actual number of columns of the matrix
  into which A is stored, i.e. the number of columns of the ancestor
  of A.")
  (:method ((a matrix-like))
    (if (parent a)
        (real-ncols (parent a))
        (nrows a))))


;;; Macro to make the actual matrix classes

(eval-when (:compile-toplevel :load-toplevel)
  
  (defmacro make-typed-matrix (fnv-type)
    "Template constructor macro for MATRIX and other object types that 
     hold data using FNV objects of type FNV-TYPE (which is the FNV 
     datatype suffix, such as complex-double, float, double, etc.)."
    (let ((fnv-ref (make-symbol* "FNV-" fnv-type "-REF"))
	  (make-fnv (make-symbol* "MAKE-FNV-" fnv-type))
          (lisp-matrix-abstract-type-name
           (make-symbol* "MATRIX-" fnv-type "-LIKE"))
	  (lisp-matrix-type-name (make-symbol* "MATRIX-" fnv-type))
	  (lisp-matrix-window-view-type-name 
	   (make-symbol* "WINDOW-MATVIEW-" fnv-type))
	  (lisp-matrix-transpose-view-type-name
	   (make-symbol* "TRANSPOSE-MATVIEW-" fnv-type))
	  (lisp-matrix-strided-view-type-name
	   (make-symbol* "STRIDED-MATVIEW-" fnv-type)))

      `(progn

         (define-abstract-class ,lisp-matrix-abstract-type-name ()
           ()
           (:documentation
            ,(format nil "Abstract class for dense matrices of type ~A"
                     fnv-type)))
         
	 (defclass ,lisp-matrix-type-name (matrix-like
                                           ,lisp-matrix-abstract-type-name)
	   ((data :initarg :data
		  ;; We don't provide an initform because it's the 
		  ;; responsibility of the MATRIX "generic" function
		  ;; to initialize the FNV.  Users aren't supposed to
		  ;; construct an element of this class directly (i.e.
		  ;; using MAKE-INSTANCE).
		  :documentation "The FNV object holding the elements."
		  :reader data))
	   (:documentation 
	    ,(format nil "Dense matrix holding elements of type ~A" fnv-type)))
         
	 (defmethod flatten-matrix-indices ((A ,lisp-matrix-type-name) i j)
           (declare (type fixnum i j))
	   ;; Note that storage is column-oriented, for compatibility
	   ;; with the BLAS and LAPACK (which are Fortran-based).
	   (+ (* (nrows A) j) i))

	 (defmethod mref ((A ,lisp-matrix-type-name) i j)
           (declare (type fixnum i j))
	   (,fnv-ref (data A) (flatten-matrix-indices A i j)))

	 (defmethod orientation ((A ,lisp-matrix-type-name))
	   :column)

         (defmethod (setf mref) (value (A ,lisp-matrix-type-name) i j)
           (declare (type fixnum i j))
           (setf (,fnv-ref (data A) (flatten-matrix-indices A i j)) value))
	 
	 (defclass ,lisp-matrix-window-view-type-name
             (window-matview ,lisp-matrix-abstract-type-name)
           ())

	 (defmethod flatten-matrix-indices ((A ,lisp-matrix-window-view-type-name) 
					    i j)
	   (declare (type fixnum i j))
	   (with-slots (parent offset0 offset1) A
	     (flatten-matrix-indices parent (+ i offset0) (+ j offset1))))

	 (defmethod mref ((A ,lisp-matrix-window-view-type-name) i j)
	   (declare (type fixnum i j))
	   (with-slots (parent offset0 offset1) A
	     (mref parent (+ i offset0) (+ j offset1))))

         (defmethod (setf mref) (value (A ,lisp-matrix-window-view-type-name) i j)
           (declare (type fixnum i j))
           (with-slots (parent offset0 offset1) A
             (setf (mref parent (+ i offset0) (+ j offset1)) value)))
         
	 (defmethod orientation ((A ,lisp-matrix-window-view-type-name))
	   (orientation (parent A)))

	 (defclass ,lisp-matrix-transpose-view-type-name
             (transpose-matview ,lisp-matrix-abstract-type-name)
           ())

	 (defmethod initialize-instance :after ((A ,lisp-matrix-transpose-view-type-name) &key)
	   "Set up the number of rows and columns correctly for a transpose view."
	   (progn
	     (setf (nrows A) (ncols (parent A)))
	     (setf (ncols A) (nrows (parent A)))))

	 (defmethod flatten-matrix-indices ((A ,lisp-matrix-transpose-view-type-name) i j)
	   (declare (type fixnum i j))
	   (flatten-matrix-indices (parent A) j i))
         
	 (defmethod mref ((A ,lisp-matrix-transpose-view-type-name) i j)
	   (declare (type fixnum i j))
	   (with-slots (parent) A
	     (mref parent j i)))

         (defmethod (setf mref) (value (A ,lisp-matrix-transpose-view-type-name) i j)
           (declare (type fixnum i j))
           (setf (mref (parent A) j i) value))

	 (defmethod orientation ((A ,lisp-matrix-transpose-view-type-name))
	   (opposite-orientation (orientation (parent A))))

	 (defclass ,lisp-matrix-strided-view-type-name
             (strided-matview ,lisp-matrix-abstract-type-name)
           ())

	 (defmethod initialize-instance :after ((A ,lisp-matrix-strided-view-type-name) &key)
	   ;; FIXME: add more error checking for the strides!
	   (assert (/= 0 (stride0 A)))
	   (assert (/= 0 (stride1 A))))

	 (defmethod flatten-matrix-indices ((A ,lisp-matrix-strided-view-type-name) 
					    i j)
	   (declare (type fixnum i j))
	   (with-slots (offset0 offset1 stride0 stride1 parent) A
	     (flatten-matrix-indices parent 
				     (+ offset0 (* i stride0))
				     (+ offset1 (* j stride1)))))

	 (defmethod mref ((A ,lisp-matrix-strided-view-type-name) i j)
	   (declare (type fixnum i j))
	   (with-slots (offset0 offset1 stride0 stride1 parent) A
	     (mref parent (+ offset0 (* i stride0)) 
		   (+ offset1 (* j stride1)))))

         (defmethod (setf mref) (value (A ,lisp-matrix-strided-view-type-name) i j)
           (declare (type (fixnum i j)))
           (with-slots (offset0 offset1 stride0 stride1 parent) A
             (setf (mref parent (+ offset0 (* i stride0))
                         (+ offset1 (* j stride1)))
                   value)))
         
	 (defmethod orientation ((A ,lisp-matrix-strided-view-type-name))
	   (orientation (parent A)))

	 (defmethod unit-stride-p ((A ,lisp-matrix-type-name))
	   t)
	 (defmethod unit-stride-p ((A ,lisp-matrix-window-view-type-name))
	   (unit-stride-p (parent A)))
	 (defmethod unit-stride-p ((A ,lisp-matrix-transpose-view-type-name))
	   (unit-stride-p (parent A)))
	 (defmethod unit-stride-p ((A ,lisp-matrix-strided-view-type-name))
	   (and (= (stride0 A) 1)
		(= (stride1 A) 1)
		(unit-stride-p (parent A))))

	 (defmethod fnv-type-to-matrix-type ((type (eql ',fnv-type))
					     matrix-type)
           (case matrix-type
             (:matrix ',lisp-matrix-type-name)
             (:window ',lisp-matrix-window-view-type-name)
             (:transpose ',lisp-matrix-transpose-view-type-name)
             (:strided ',lisp-matrix-strided-view-type-name)
             (:base ',lisp-matrix-abstract-type-name)
             (t
              (error "Invalid matrix type ~A" matrix-type))))

	 (defmethod matrix-type-to-fnv-type
             ((type (eql ',lisp-matrix-type-name)))
	   ',fnv-type)
	 (defmethod matrix-type-to-fnv-type
             ((type (eql ',lisp-matrix-window-view-type-name)))
	   ',fnv-type)
	 (defmethod matrix-type-to-fnv-type
             ((type (eql ',lisp-matrix-transpose-view-type-name)))
	   ',fnv-type)
	 (defmethod matrix-type-to-fnv-type
             ((type (eql ',lisp-matrix-strided-view-type-name)))
	   ',fnv-type)

	 (defmethod fnv-type ((A ,lisp-matrix-type-name))
	   ',fnv-type)
	 (defmethod fnv-type ((A ,lisp-matrix-window-view-type-name))
	   ',fnv-type)
	 (defmethod fnv-type ((A ,lisp-matrix-transpose-view-type-name))
	   ',fnv-type)
	 (defmethod fnv-type ((A ,lisp-matrix-strided-view-type-name))
	   ',fnv-type)

         (defmethod make-matrix (nrows ncols (fnv-type (eql ',fnv-type)) &key
                                 (initial-element 0)
                                 (initial-contents nil initial-contents-p))
           ;; Logic for handling initial contents or initial element.
           ;; Initial contents take precedence (if they are specified,
           ;; then any supplied initial-element argument is ignored).
           (let ((data
                  (cond
                    (initial-contents-p
                     (let* ((n (* nrows ncols))
                            (fnv (,make-fnv n)))
                       (etypecase initial-contents
                         ;; FIXME: check array size -- Evan Monroig
                         ;; 2008-05-04
                         (array
                          (dotimes (i nrows)
                            (dotimes (j ncols)
                              (setf (,fnv-ref fnv (+ i (* nrows j)))
                                    (aref initial-contents i j)))))
                         ;; FIXME: check list size -- Evan Monroig
                         ;; 2008-05-04
                         (list
                          (loop for i below nrows
                             for row in initial-contents do
                             (loop for j below ncols
                                for cell in row do
                                (setf (,fnv-ref fnv (+ i (* nrows j)))
                                      cell)))))
                       fnv))
                    (t
                     (,make-fnv (* nrows ncols)
                                :initial-value initial-element)))))
             (make-instance ',lisp-matrix-type-name
                            :nrows nrows
                            :ncols ncols
                            :data data)))))))


(defmethod data ((a matview))
  (data (parent A)))

(defgeneric m= (a b)
  (:documentation "Test for strict equality of dimensions and of each
  matrix element of A and B."))

(defmethod m= ((a matrix-like) (b matrix-like))
  (and (= (nrows a) (nrows b))
       (= (ncols a) (ncols b))
       (dotimes (i (nrows a) t)
         (dotimes (j (ncols b))
           (unless (= (mref a i j) (mref b i j))
             (return-from m= nil))))))

(defmethod print-object ((a matrix-like) stream)
  (print-unreadable-object (a stream :type t)
    (format stream " ~d x ~d" (nrows a) (ncols a))
    (dotimes (i (nrows a))
      (terpri stream)
      (dotimes (j (ncols a))
        (write-char #\space stream)
        (write (mref a i j) :stream stream)))))

;;; Instantiate the classes and methods.
(make-typed-matrix double)
(make-typed-matrix float)
(make-typed-matrix complex-double)
(make-typed-matrix complex-float)

	       




;;; "Generic" methods for creating views.

;; FIXME: OFFSET0 and OFFSET1 should be changed to make more obvious
;; that is is for rows and columns
(defgeneric window (parent &key nrows ncols offset0 offset1)
  (:documentation "Creates a window view of the given matrix-like
  object PARENT.  Note that window views always have the same
  orientation as their parents.")
  (:method ((parent matrix-like) 
            &key (nrows (nrows parent))
            (ncols (ncols parent))
            (offset0 0)
            (offset1 0))
    (check-type nrows (integer 0))
    (check-type ncols (integer 0))
    (check-type offset0 (integer 0))
    (check-type offset1 (integer 0))
    (assert (<= (+ offset0 nrows) (nrows parent)))
    (assert (<= (+ offset1 ncols) (ncols parent)))
    (make-instance (fnv-type-to-matrix-type (fnv-type parent) :window)
                   :parent parent
                   :nrows nrows
                   :ncols ncols
                   :offset0 offset0
                   :offset1 offset1)))

(defgeneric transpose (parent)
  (:documentation "Creates a transpose view of the given matrix-like
  object PARENT.")
  (:method ((parent matrix-like))
    (make-instance (fnv-type-to-matrix-type (fnv-type parent) :transpose)
                   :parent parent)))

(defgeneric strides (parent &key nrows ncols offset0 offset1 stride0
                            stride1)
  (:documentation "Creates a strided view of the given matrix-like
  object PARENT.")
  (:method ((parent matrix-like)
            &key 
            (nrows (nrows parent))
            (ncols (ncols parent))
            (offset0 0)
            (offset1 0)
            (stride0 1)
            (stride1 1))
    (check-type nrows (integer 0))
    (check-type ncols (integer 0))
    (check-type offset0 (integer 0))
    (check-type offset1 (integer 0))
    (check-type stride0 (integer 1))
    (check-type stride1 (integer 1))
    (assert (<= (+ offset0 (* stride0 (1- nrows))) (nrows parent)))
    (assert (<= (+ offset1 (* stride1 (1- ncols))) (ncols parent)))
    (make-instance (fnv-type-to-matrix-type (fnv-type parent) :strided)
                   :parent parent
                   :nrows nrows
                   :ncols ncols
                   :offset0 offset0
                   :offset1 offset1
                   :stride0 stride0
                   :stride1 stride1)))

;;; Export the symbols that we want to export.
(export '(nelts matrix window transpose strides matrix-dimension matrix-dimensions mref unit-stride-p))

