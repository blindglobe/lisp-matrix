;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Time-stamp: <2008-06-11 14:57:16 Evan Monroig>

(in-package :lisp-matrix)

(declaim (optimize (debug 3)))

;;;; This is a rewrite of the matrix class interface, in order to
;;;; allow both foreign arrays and lisp arrays as underlying
;;;; implementation.
;;;;
;;;; * The base MATRIX-LIKE class
;;;; 
;;;; We will begin by defining what is a matrix, without assuming
;;;; anything about the underlying implementation.
;;;;
;;;; We define the abstract matrix class, and some conditions
;;;; satisfied by its slots are checked at initialization.
;;;; Conceptually, a matrix is just a table with a certain number of
;;;; rows and columns.  We define a generic function DATA that will
;;;; return the underlying data of the matrix, but leave it to be
;;;; defined in the implementation classes.
;;;;
;;;; We define generic functions for the slot readers in order to
;;;; provide them documentation and make more explicit the fact that
;;;; they will be shared for other functions as well.
;;;;
;;;; Note that we live all error checking of class instanciation to
;;;; the functions that create such matrices.  (see the section
;;;; "Creating Matrices")

(defclass matrix-like ()
  ((nrows :initarg :nrows
          :reader nrows
          :initform 0)
   (ncols :initarg :ncols
          :reader ncols
          :initform 0))
  (:documentation "Abstract base class for 2-D matrices and matrix
   views.  We assume for now that matrices are stored in column
   order (Fortran style), for BLAS and LAPACK compatibility."))

(defgeneric nrows (matrix)
  (:documentation "Number of rows in the matrix."))

(defgeneric ncols (matrix)
  (:documentation "Number of columns in the matrix."))

(defgeneric data (matrix)
  (:documentation "Return the underlying data of the matrix."))

;;;; ** Dimensions
;;;; 
;;;; Based on this information, we can already define some generic
;;;; functions which give us some information on the dimensions and
;;;; number of elements.  We will model the functions as much as
;;;; possible on the array interface.

(defgeneric nelts (matrix)
  (:documentation "Default method for computing the number of elements
  of a matrix.  For obvious reasons, this will be overridden for
  subclasses that implement sparse matrices.")
  (:method ((matrix matrix-like)) (* (nrows matrix) (ncols matrix))))

(defgeneric matrix-dimension (matrix axis-number)
  (:documentation "Like ARRAY-DIMENSION for matrix-like objects.")
  (:method ((matrix matrix-like) axis-number)
    (cond ((= axis-number 0) (nrows matrix))
          ((= axis-number 1) (ncols matrix))
          (t (error "Invalid AXIS-NUMBER for MATRIX ~A: ~A" matrix
                    axis-number)))))

(defgeneric matrix-dimensions (matrix)
  (:documentation "Like ARRAY-DIMENSIONS for matrix-like objects.")
  (:method ((matrix matrix-like)) 
    (list (nrows matrix) (ncols matrix))))

;;;; ** Element indexing
;;;;
;;;; Now that we know the dimensions of a matrix, we want to know how
;;;; to access the elements.  This depends on the storage format of
;;;; the matrix.
;;;;
;;;; After the basic design of this library has been worked out, we
;;;; may add different storage formats, such as :general, :upper,
;;;; :lower, (:banded lbw ubw), etc.
;;;;
;;;; But for now we restrict ourselves to dense matrix which are
;;;; stored in 1D arrays in row-major or column-major format.  This
;;;; means that a matrix will have an orientation, which is one of
;;;;:ROW and :COLUMN.

(defun valid-orientation-p (orientation)
  (member orientation '(:row :column)))

(defun opposite-orientation (orientation)
  (case orientation
    (:column :row)
    (:row :column)
    (t (error "Invalid orientation ~A" orientation))))

;;;; We then define four generic functions for matrices, that should
;;;; be specialized on for subclasses of MATRIX-LIKE.

(defgeneric orientation (matrix)
  (:documentation "lisp-matrix objects are stored by default in
  column-major order for compatibility with the BLAS and LAPACK, which
  are Fortran-based.")
  (:method ((matrix matrix-like)) :column))

(defgeneric flatten-matrix-indices (matrix i j)
  (:documentation "Given an index pair (I,J) into the given matrix
  MATRIX, returns the 1-D index corresponding to the location in the
  underlying storage in which the element MATRIX(i,j) is stored.

  The default implementation is for a column-major dense storage,
  corresponding to the default implementation of the generic function
  ORIENTATION.")
  (:method ((matrix matrix-like) i j)
    (+ i (* j (nrows matrix)))))

(defgeneric mref (matrix i j)
  (:documentation "(MREF MATRIX I J) gives you the (I,J)-th element of
  MATRIX.  This method is slow as it requires CLOS method dispatch and
  index calculation(s), and should thus be replaced with vectorized or
  block operations whenever possible."))

(defgeneric (setf mref) (value matrix i j)
  (:documentation "Set the (I,J)-th element of MATRIX to VALUE.  This
  method is slow as it requires CLOS method dispatch and index
  calculation(s), and should thus be replaced with vectorized or block
  operations whenever possible."))

;;;; * Matrix views (MATVIEW)
;;;;
;;;; A matrix view of a matrix A is a new matrix whose underlying data
;;;; is the same as that of A but are viewed through a filter.  We
;;;; call A the "parent" matrix of the view.
;;;;
;;;; Also, it is common that a MATVIEW matrix does not have the same
;;;; number of rows and columns as its parent, but we wish 

(defclass matview (matrix-like) 
  ((parent :initarg :parent
	   :reader parent
	   :documentation "The \"parent\" object to which this matrix
	   view relates."))
  (:documentation "An abstract class representing a \"view\" into a
  matrix.  That view may be treated as a (readable and writeable)
  reference to the elements of the matrix."))

(defgeneric matview-p (matrix)
  (:documentation "Is MATRIX a MATVIEW?")
  (:method ((matrix matrix-like)) nil)
  (:method ((matrix matview)) t))

(defgeneric parent (matrix)
  (:documentation "For a MATVIEW matrix, returns the parent matrix to
  which this matrix view relates.")
  (:method ((matrix matrix-like)) nil))

(defgeneric ancestor (matrix)
  (:documentation "For a MATVIEW matrix, return the ancestor matrix to
  which this matrix view relates.")
  (:method ((matrix matrix-like)) matrix)
  (:method ((matrix matview)) (ancestor (parent matrix))))

(defgeneric real-nrows (matrix)
  (:documentation "Return the actual number of rows of the matrix into
  which A is stored, i.e. the number of rows of the ancestor of A.")
  (:method ((matrix matrix-like)) (nrows matrix))
  (:method ((matrix matview)) (nrows (ancestor matrix))))

(defgeneric real-ncols (matrix)
  (:documentation "Return the actual number of columns of the matrix
  into which A is stored, i.e. the number of columns of the ancestor
  of A.")
  (:method ((matrix matrix-like)) (ncols matrix))
  (:method ((matrix matview)) (ncols (ancestor matrix))))

(defmethod orientation ((matrix matview))
  (orientation (parent matrix)))

(defmethod data ((matrix matview))
  (data (parent matrix)))

;;;; ** Transposed matrix (TRANSPOSE-MATVIEW)

(defclass transpose-matview (matview)
  ()
  (:documentation "A TRANSPOSE-MATVIEW views the transpose of a
  matrix.  If you want a deep copy, call the appropriate COPY function
  on the transpose view.  The reason for this is to avoid expensive
  operations whenever possible.  Many BLAS and LAPACK routines have a
  \"TRANSA\" argument (or similar) that lets you specify that the
  operation should work with the transpose of the matrix.  This means
  that it usually isn't necessary to compute an explicit transpose, at
  least for input arguments."))

(defgeneric transposed-p (matrix)
  (:documentation "Is MATRIX a transposed view of its ancestor
  matrix?")
  (:method ((matrix matrix-like)))
  (:method ((matrix matview)) (transposed-p (parent matrix)))
  (:method ((matrix transpose-matview)) t))

(defmethod orientation ((matrix transpose-matview))
  (opposite-orientation (orientation (parent matrix))))

(defmethod flatten-matrix-indices ((matrix transpose-matview) i j)
  (flatten-matrix-indices (parent matrix) j i))

(defmethod mref ((matrix transpose-matview) i j)
  (mref (parent matrix) j i))

(defmethod (setf mref) (value (matrix transpose-matview) i j)
  (setf (mref (parent matrix) j i) value))

;;;; ** Windowed matrix (WINDOW-MATVIEW)

(defclass window-matview (matview)
  ((row-offset :initarg :row-offset
               :initform 0
               :reader row-offset)
   (col-offset :initarg :col-offset
               :initform 0
               :reader col-offset))
  (:documentation "A WINDOW-MATVIEW views a block of elements in the
  underlying matrix that is conceptually 2-D contiguous.  If the
  underlying matrix is column-oriented, the elements in each column of
  a WINDOW-MATVIEW are stored contiguously, and horizontally adjacent
  elements are separated by a constant stride (\"LDA\" in BLAS
  terms)."))

(defgeneric zero-offset-p (matrix)
  (:documentation "Has MATRIX a zero offset (as for window and stride
  views)?")
  (:method ((matrix matrix-like)) t)
  (:method ((matrix matview)) (zero-offset-p (parent matrix)))
  (:method ((matrix window-matview))
    (and (= 0 (row-offset matrix) (col-offset matrix))
         (zero-offset-p (parent matrix)))))

(defgeneric offset (matrix)
  (:documentation "Offset of the first element of MATRIX.")
  (:method ((matrix matrix-like))
    (flatten-matrix-indices matrix 0 0)))

(defmethod flatten-matrix-indices ((matrix window-matview) i j)
  (flatten-matrix-indices (parent matrix)
                          (+ i (row-offset matrix))
                          (+ j (col-offset matrix))))

(defmethod mref ((matrix window-matview) i j)
  (mref (parent matrix)
        (+ i (row-offset matrix))
        (+ j (col-offset matrix))))

(defmethod (setf mref) (value (matrix window-matview) i j)
  (setf (mref (parent matrix)
              (+ i (row-offset matrix))
              (+ j (col-offset matrix)))
        value))

;;;; ** Strided matrix

(defclass strided-matview (window-matview)  
  ((row-stride :initarg :row-stride
               :initform 1
               :reader row-stride
               :documentation "Stride in the row direction")
   (col-stride :initarg :col-stride
               :initform 1
               :reader col-stride
               :documentation "Stride in the column direction"))
  (:documentation "A STRIDED-MATVIEW views a window of the matrix with
  a particular stride in each direction (the stride can be different
  in each direction)."))

(defgeneric unit-stride-p (matrix)
  (:documentation "Tests for \"unit stride.\" (The strided matrix view
  is the only view which causes itself and its children possibly not
  to have unit stride.)")
  (:method ((matrix matrix-like)) t)
  (:method ((matrix matview)) (unit-stride-p (parent matrix)))
  (:method ((matrix strided-matview))
    (and (= 1 (row-stride matrix) (col-stride matrix))
         (unit-stride-p (parent matrix)))))

(defmethod flatten-matrix-indices ((matrix strided-matview) i j)
  (flatten-matrix-indices (parent matrix)
                          (+ (row-offset matrix) (* i (row-stride matrix)))
                          (+ (col-offset matrix) (* j (col-stride matrix)))))

(defmethod mref ((matrix strided-matview) i j)
  (mref (parent matrix)
        (+ (row-offset matrix) (* i (row-stride matrix)))
        (+ (col-offset matrix) (* j (col-stride matrix)))))

(defmethod (setf mref) (value (matrix strided-matview) i j)
  (setf (mref (parent matrix)
              (+ (row-offset matrix) (* i (row-stride matrix)))
              (+ (col-offset matrix) (* j (col-stride matrix))))
        value))

;;;; * Creating matrices
;;;;
;;;; Now that we have a description of the matrix classes, we need
;;;; some way to make instances of them.  We assume that we have
;;;; several implementations of matrices.  The two that come to mind
;;;; are :LISP-ARRAY (matrices based on lisp arrays) and
;;;; :FOREIGN-ARRAY (matrices based on foreign arrays allocated
;;;; through FFI).
;;;;
;;;; ** Simple matrices
;;;; 
;;;; We define two functions to create simple matrices: MAKE-MATRIX*
;;;; which is a generic function and is specialized on each
;;;; implementation, and MAKE-MATRIX which is a wrapper over it and
;;;; has the target implementation as keyword argument.

(defparameter *default-implementation* :lisp-array
  "One of :LISP-ARRAY (use FFA package) or :FOREIGN-ARRAY (uses FNV
  package).")

(defparameter *default-element-type* 'double-float)

(defgeneric make-matrix* (nrows ncols implementation &key element-type
                                initial-element)
  (:documentation "Create a NROWS x NCOLS matrix with IMPLEMENTATION
  as underlying matrix implementation.  ELEMENT-TYPE is the lisp type
  to be stored in the matrix, and INITIAL-ELEMENT an element that may
  be used to initially fill the matrix.

  If INITIAL-ELEMENT is not specified, the matrix is not initialized,
  and accessing its elements will thus return spurious values."))

(defun make-matrix (nrows ncols &key
                    (implementation *default-implementation*)
                    (element-type *default-element-type*)
                    (initial-element nil initial-element-p)
                    (initial-contents nil initial-contents-p))
  "Create a NROWS x NCOLS matrix with
  MATRIX-IMPLEMENTATION as underlying implementation.  ELEMENT-TYPE is
  the lisp type to be stored in the matrix, and INITIAL-ELEMENT an
  element that may be used to initially fill the matrix.

  If INITIAL-ELEMENT is not specified, the matrix is not initialized,
  and accessing its elements will thus return spurious values.

  If INITIAL-CONTENTS is specified, it is used to initialize the
  matrix, by using the generic function COPY!.

  MATRIX-IMPLEMENTATION can be one of :LISP-ARRAY and :FOREIGN-ARRAY"
  (when (and initial-element-p initial-contents-p)
    (error "Both INITIAL-ELEMENT and INITIAL-CONTENTS should not be ~
    specified"))
  (let ((matrix (apply #'make-matrix* nrows ncols implementation
                       :element-type element-type
                       (when initial-element-p
                         (list :initial-element initial-element)))))
    (when initial-contents
      (copy! initial-contents matrix))
    matrix))

;;;; Because we will also need to access the element-type and
;;;; implementation of a matrix, we also define the corresponding
;;;; generic functions.
;;;;
;;;; By default, the MATVIEW types will not directly have the data and
;;;; ask their parents what implementation or element-type they are.
;;;; But a generic MATRIX-LIKE object will not know and therefore you
;;;; need to specialize these two functions for each implementation.

(defgeneric implementation (matrix)
  (:documentation "Underlying implementation of MATRIX.")
  (:method ((matrix matview))
    (implementation (parent matrix))))

(defgeneric element-type (matrix)
  (:documentation "Type of the elements of MATRIX.")
  (:method ((matrix matview))
    (element-type (parent matrix))))

(defgeneric element-type-size (matrix)
  (:documentation "Size in memory of the elements of MATRIX (useful
  for pointer arithmetic when calling foreign functions)."))

;;;; ** Matrix views
;;;;
;;;; We define three generic function to create the transposed,
;;;; windowed and strided views.  By default, the views are not tied
;;;; to a particular implementation, but an implementation may want to
;;;; define specific MATVIEW subclasses.  Therefore, we introduce
;;;; generic functions to obtain the correct MATVIEW class to use.

(defgeneric transpose-class (matrix)
  (:documentation "Return the name of the class to be used for a
  transpose of MATRIX.")
  (:method ((matrix matrix-like)) 'transpose-matview))

(defgeneric window-class (matrix)
  (:documentation "Return the name of the class to be used for a
  window of MATRIX.")
  (:method ((matrix matrix-like)) 'window-matview))

(defgeneric stride-class (matrix)
  (:documentation "Return the name of the class to be used for a
  stride of MATRIX.")
  (:method ((matrix matrix-like)) 'strided-matview))

(defgeneric transpose (matrix)
  (:documentation "Creates a transpose view of the given matrix-like
  object MATRIX.")
  (:method ((matrix matrix-like))
    (make-instance (transpose-class matrix)
                   :parent matrix
                   :nrows (ncols matrix)
                   :ncols (nrows matrix))))

(defgeneric window (matrix &key nrows ncols row-offset col-offset)
  (:documentation "Creates a window view of the given matrix-like
  object MATRIX.  Note that window views always have the same
  orientation as their parents.")
  (:method ((matrix matrix-like) 
            &key (nrows (nrows matrix))
            (ncols (ncols matrix))
            (row-offset 0)
            (col-offset 0))
    (check-type nrows (integer 0))
    (check-type ncols (integer 0))
    (check-type row-offset (integer 0))
    (check-type col-offset (integer 0))
    (assert (<= (+ row-offset nrows) (nrows matrix)))
    (assert (<= (+ col-offset ncols) (ncols matrix)))
    (make-instance (window-class matrix)
                   :parent matrix
                   :nrows nrows
                   :ncols ncols
                   :row-offset row-offset
                   :col-offset col-offset)))

(defgeneric strides (matrix &key nrows ncols row-offset col-offset row-stride
                            col-stride)
  (:documentation "Creates a strided view of the given matrix-like
  object MATRIX.")
  (:method ((matrix matrix-like)
            &key 
            (nrows (nrows matrix))
            (ncols (ncols matrix))
            (row-offset 0)
            (col-offset 0)
            (row-stride 1)
            (col-stride 1))
    (check-type nrows (integer 0))
    (check-type ncols (integer 0))
    (check-type row-offset (integer 0))
    (check-type col-offset (integer 0))
    (check-type row-stride (integer 1))
    (check-type col-stride (integer 1))
    (assert (<= (+ row-offset (* row-stride (1- nrows))) (nrows matrix)))
    (assert (<= (+ col-offset (* col-stride (1- ncols))) (ncols matrix)))
    (make-instance (stride-class matrix)
                   :parent matrix
                   :nrows nrows
                   :ncols ncols
                   :row-offset row-offset
                   :col-offset col-offset
                   :row-stride row-stride
                   :col-stride col-stride)))

;;;; ** Specific matrices

(defun ones (nrows ncols &key
             (implementation *default-implementation*)
             (element-type *default-element-type*))
  "Create a NROWS x NCOLS matrix filled with ones."
  (make-matrix nrows ncols :implementation implementation
               :element-type element-type
               :initial-element (coerce 1 element-type)))

(defun zeros (nrows ncols &key
              (implementation *default-implementation*)
              (element-type *default-element-type*))
  "Create a NROWS x NCOLS matrix filled with zeros."
  (make-matrix nrows ncols :implementation implementation
               :element-type element-type
               :initial-element (coerce 0 element-type)))

(defun eye (nrows ncols &key
            (implementation *default-implementation*)
            (element-type *default-element-type*))
  "Create a NROWS x NCOLS matrix with ones on the diagonal, and zeros
  elsewhere."
  (let ((matrix (zeros nrows ncols :implementation implementation
                       :element-type element-type)))
    (let ((n (min nrows ncols))
          (one (coerce 1 element-type)))
      (dotimes (i n)
        (setf (mref matrix i i) one))
      matrix)))

(defun rand (nrows ncols &key
             (implementation *default-implementation*)
             (element-type *default-element-type*)
             (state *random-state*))
  "Create a NROWS x NCOLs matrix filled with uniformly distributed
  pseudo-random numbers between 0 and 1."
  ;; FIXME: doesn't work for complex types
  (check-type state random-state)
  (let ((matrix (make-matrix nrows ncols
                             :implementation implementation
                             :element-type element-type))
        (one (coerce 1 element-type)))
    (dotimes (i nrows)
      (dotimes (j ncols)
        (setf (mref matrix i j) (random one state))))
    matrix))

;;;; * Matrix operations
;;;;
;;;; Once we have described the general interface, we can use it to
;;;; implement some basic operations on the matrices.

;;;; ** Copying

(defgeneric copy! (a b)
  (:documentation "Copy A into B if they are not the same object, and
  return B.  A and B should be matrices with the same dimensions, but
  not necessarily of the same implementation."))

(defmethod copy! ((a matrix-like) (b matrix-like))
  (assert (= (ncols a) (ncols b)))
  (assert (= (nrows a) (nrows b)))
  (assert (subtypep (element-type a) (element-type b)))
  ;; FIXME: care about fast copy once everything is working
  (unless (eq a b)
    (dotimes (i (nrows a))
      (dotimes (j (ncols a))
        (setf (mref b i j) (mref a i j)))))
  b)

(defmethod copy! ((a array) (b matrix-like))
  (unless (and (= (array-rank a) 2)
               (= (array-dimension a 0) (nrows b))
               (= (array-dimension a 1) (ncols b)))
    (error "A doesn't have the correct dimensions"))
  ;; We cannot directly check ARRAY-ELEMENT-TYPE because for example
  ;; for integers it is upgraded to T, so we check the element type
  ;; individually for each element.
  (let ((element-type (element-type b)))
    (dotimes (i (nrows b))
      (dotimes (j (ncols b))
        (assert (typep (aref a i j) element-type))
        (setf (mref b i j) (aref a i j)))))
  b)

(defmethod copy! ((a list) (b matrix-like))
  (unless (and (= (nrows b) (length a))
               (= (ncols b) (length (first a))))
    (error "A doesn't have the correct dimensions"))
  (let ((element-type (element-type b)))
    (loop for i below (nrows b) for row in a do
          (loop for j below (ncols b) for cell in row do
                (assert (typep cell element-type))
                (setf (mref b i j) cell))))
  b)

(declaim (inline copy))
(defun copy (matrix)
  "Return a deep copy of MATRIX with the same implementation and
  element-type."
  (copy* matrix (implementation matrix)))

(defgeneric copy* (matrix implementation)
  (:documentation "Same as COPY but specify the implementation.")
  (:method ((matrix matrix-like) implementation)
    (make-matrix (nrows matrix) (ncols matrix)
                 :implementation implementation
                 :element-type (element-type matrix)
                 :initial-contents matrix)))

(declaim (inline copy-maybe))
(defun copy-maybe (matrix test)
  "Return a deep copy of MATRIX if TEST is satisfied, or return MATRIX
  itself.  TEST is a function of one argument that will be applied to
  MATRIX."
  (copy-maybe* matrix test (implementation matrix)))

(defun copy-maybe* (matrix test implementation)
  "Same as COPY-MAYBE but specify the implementation."
  (if (funcall test matrix)
      (copy* matrix implementation)
      matrix))

(defgeneric fill-matrix (matrix fill-element)
  (:documentation "Set each element of MATRIX to FILL-ELEMENT.")
  (:method :before ((matrix matrix-like) fill-element)
    (assert (typep fill-element (element-type matrix))))
  (:method ((matrix matrix-like) fill-element)
    (dotimes (i (nrows matrix))
      (dotimes (j (ncols matrix))
        (setf (mref matrix i j) fill-element)))))

;;;; ** Equality

(defgeneric m= (a b)
  (:documentation "Test for strict equality of dimensions and of each
  matrix element of A and B."))

(defmethod m= ((a matrix-like) (b matrix-like))
  ;; Note: this will not work for matrices of things that are not
  ;; numbers
  (and (= (nrows a) (nrows b))
       (= (ncols a) (ncols b))
       (dotimes (i (nrows a) t)
         (dotimes (j (ncols b))
           (unless (= (mref a i j) (mref b i j))
             (return-from m= nil))))))

;;;; ** Printing

(defmethod print-object ((a matrix-like) stream)
  (print-unreadable-object (a stream :type t)
    (format stream " ~d x ~d" (nrows a) (ncols a))
    (dotimes (i (nrows a))
      (terpri stream)
      (dotimes (j (ncols a))
        (write-char #\space stream)
        (write (mref a i j) :stream stream)))))

;;; Local Variables:
;;; outline-regexp: ";;;; \\*\\|("
;;; End:
