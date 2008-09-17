;;;; Recall that CALL-NEXT-METHOD for regular (i.e. "primary", 
;;;; that is not before, after or around) DEFMETHODs goes from 
;;;; a more specialized method to a less specialized method.  If 
;;;; you don't call CALL-NEXT-METHOD, then the more specialized 
;;;; method has to take care of everything.
;;;;

;;;; :BEFORE methods are also run in most-specific-first order, but
;;;; before the primary method, and :BEFORE methods don't have to 
;;;; call CALL-NEXT-METHOD.

;;;; However, :AFTER methods are run in the reverse order -- from
;;;; least specific to most specific.


(defpackage #:lisp-matrix
  (:nicknames #:mat)
  (:use #:common-lisp #:cffi #:types #:array-backend))
(provide "lisp-matrix")
(in-package #:lisp-matrix)


;;;; FUNCTIONS FOR HANDLING :orientation

;;; since we only consider matrices, orientations only denote the
;;; approach for creating a true ordering for mapping between an
;;; "abstracted" array data structure to/from a vector storing an
;;; array (i.e. LAPACK/BLAS and similar fortran storage)

(defun valid-orientation-p (orientation)
  "Returns T iff ORIENTATION is a valid orientation for a MATRIX-LIKE object."
  (or (eq orientation :column) (eq orientation :row)))

(defun opposite-orientation (orientation)
  "Returns the opposite orientation from the given ORIENTATION."
  (cond ((eq orientation :column)
	 :row)
	((eq orientation :row)
	 :column)
	(t (error "Invalid orientation ~A" orientation))))


;;;; BASE CLASSES

(defmacro define-abstract-class (classname super-list &body body)
  "A wrapper for DEFCLASS that lets you define abstract base classes.
   If you try to instantiate an object of this class, an error is signaled."
  `(progn
     (defclass ,classname ,super-list ,@body)

     ;; Protection so that abstract classes aren't instantiated.
     ;; We can take this out later using a compile-time constant
     ;; (or even check the optimization options and remove it if
     ;; SAFETY is set low enough).
     (defmethod initialize-instance :before ((x ,classname) &key)
       (if (typep x ,classname)
	   (error "~A is an abstract base class and not to be instantiated."
		  (quote ,classname))))))


(define-abstract-class data-container ()
  (:documentation "Abstract base class for everything that holds data of a particular single C type.")
  ((data :initarg :data
	 :documentation "The array (possibly foreign) in which the elements are stored"
	 :reader data)
  (element-type :initarg :element-type
		:documentation "CFFI type of the elements accessible via this data structure."
		:reader element-type)))

(defmethod initialize-instance :after ((x data-container) &key)
  "Canonicalize the element type to the appropriate Lisp type.  This
will report an error if the element type isn't valid."
  (setf (slot-value x 'element-type) (cffitype->lisp (element-type x))))

(define-abstract-class vector-like (data-container)
  (:documentation "Abstract base class for 1-D vectors and vector views.")
  ((nelts :initarg :nelts
	  :documentation "Number of elements in this vector (view)."
	  :reader nelts)))

(defmethod initialize-instance :after ((x vector-like) &key)
  (if (< (nelts x) 0)
      (error "VECTOR-LIKE objects cannot have a negative number ~A of elements."
	     (nelts x))))


;; Defn: Stride refers to an offset which is used to handle the mapping
;; of vectors (actual storage) to matrices (presented-to-user data
;; structure type).  Commonly used to offset to adjacent rows
;; (columns) in a column-oriented (row-oriented) mapping.

(defclass vec (vector-like)
  (:documentation "A 1-D vector with unit stride storage."))


(defmacro fix-vec-data (data nelts element-type &key (copy-always t))
  "
  Given DATA, which is some container holding the data that we want
  in our VEC object, gets that data somehow into an array of the 
  appropriate type (C or Lisp) in our VEC object.  That may or may
  not involve copying, unless COPY-ALWAYS is non-NIL, in which case
  it ALWAYS involves copying.
  "
  (let ((cffitype (gensym))
	(lisptype (gensym))
	(a (gensym))
	(e (gensym))
	(i (gensym))
	(num-elts (gensym))
	(cffitype-expr (if (constantp element-type) 
			   (lisptype->cffi element-type)
			   `(lisptype->cffi ,element-type)))
	(lisptype-expr (if (constantp element-type)
			   (cffitype->lisp element-type)
			   `(cffitype->lisp ,element-type))))
    (if (has-feature-p :use-lisp-arrays)
	(cond ((has-feature-p :static-arrays)
	       `(let* ((,num-elts ,nelts)
		       (,cffitype ,cffitype-expr)
		       (,a (ll-make-array ,num-elts ,cffitype)))
		  (cond ((typep ,data 'cffi:pointer)
			 (copy-into-lisp ,a ,data ,cffitype ,num-elts))
			((typep ,data 'array)
			 ;; As there generally isn't a way to test for 
			 ;; static arrays, assume that the given array
			 ;; isn't static, and copy it.  (Even if we can
			 ;; turn off the GC, the cost of doing that is 
			 ;; probably high, especially in a multithreaded
			 ;; Lisp, so it probably isn't worth it not to 
			 ;; copy.)
			 (progn
			   (replace ,a ,data :start2 0 :end2 (1- ,num-elts))
			   a))
			((listp ,data)
			 ;; DATA is a list.  Use COERCE on all the elements
			 ;; to make sure that they have the right type.  If
			 ;; they don't, an error will be signaled.
			 (let ((,lisptype ,lisptype-expr))
			   (loop for ,e in ,data 
			      for ,i from 0 to (1- ,num-elts) do
				(declare (type (simple-array ,lisptype (*)) ,a))
				(setf (aref ,a ,i) (coerce ,e ,lisptype))
			      finally (return ,a))))
			(t (error "DATA must be a CFFI C pointer, Lisp array or Lisp list.")))))
	      ((has-feature-p :can-pause-gc)
	       (if copy-always
		   `(let* ((,num-elts ,nelts)
			   (,cffitype ,cffitype-expr)
			   (,a (ll-make-array ,num-elts ,cffitype)))
		      (cond ((typep ,data 'cffi:pointer)
			     (copy-into-lisp ,a ,data ,cffitype ,num-elts))
			    ((typep ,data 'array)
			     (progn
			       (replace ,a ,data :start2 0 :end2 (1- ,num-elts))
			       a))
			    ((listp ,data)
			     ;; DATA is a list.  Use COERCE on all the elements
			     ;; to make sure that they have the right type.  If
			     ;; they don't, an error will be signaled.
			     (let ((,lisptype (cffitype->lisp ,element-type)))
			       (loop for ,e in ,data 
				  for ,i from 0 to (1- ,num-elts) do
				    (declare (type (simple-array ,lisptype (*)) ,a))
				    (setf (aref ,a ,i) (coerce ,e ,lisptype))
				  finally (return ,a))))
			    (t (error "DATA must be a CFFI C pointer, Lisp array or Lisp list."))))
		   ;; Don't copy if we're given a Lisp array.
		   `(let* ((,num-elts ,nelts)
			   (,cffitype ,cffitype-expr))
		      (cond ((typep ,data 'cffi:pointer)
			     (copy-into-lisp (ll-make-array ,num-elts ,cffitype) ,data ,cffitype ,num-elts))
			    ((typep ,data 'array)
			     ;; Shallow copy (on purpose!)
			     ,data)
			    ((listp ,data)
			     ;; DATA is a list.  Use COERCE on all the elements
			     ;; to make sure that they have the right type.  If
			     ;; they don't, an error will be signaled.
			     (let ((,lisptype (cffitype->lisp ,element-type))
				   (,a (ll-make-array ,num-elts ,cffitype)))
			       (loop for ,e in ,data 
				  for ,i from 0 to (1- ,num-elts) do
				    (declare (type (simple-array ,lisptype (*)) ,a))
				    (setf (aref ,a ,i) (coerce ,e ,lisptype))
				  finally (return ,a))))
			    (t (error "DATA must be a CFFI C pointer, Lisp array or Lisp list."))))))
	      (t (error "Hey, I don't think you should use Lisp arrays if you can't pin them or stop GC's.  You should check the *our-features* list in features.lisp and remove the :use-lisp-arrays feature.")))
	;; Return a C array.
	(if copy-always
	    `(let* ((,num-elts ,nelts)
		    (,cffitype (lisptype->cffi ,element-type))
		    (,a (ll-make-array ,num-elts ,cffitype)))
	       (cond ((typep ,data 'cffi:pointer)
		      (dotimes (,i ,num-elts ,a)
			(setf (ll-aref ,a ,cffitype ,i) (ll-aref ,data ,cffitype ,i))))
		     ((typep ,data 'array)
		      (copy-into-foreign ,a ,data ,cffitype ,num-elts))
		     ((listp ,data)
		      ;; DATA is a list.  Use COERCE on all the elements
		      ;; to make sure that they have the right type.  If
		      ;; they don't, an error will be signaled.
		      (let ((,lisptype (cffitype->lisp ,element-type)))
			(loop for ,e in ,data 
			   for ,i from 0 to (1- ,num-elts) do
			     (setf (ll-aref ,a ,cffitype ,i) (coerce ,e ,lisptype))
			   finally (return ,a))))
		     (t (error "DATA must be a CFFI C pointer, Lisp array or Lisp list."))))
	    ;; Don't copy DATA if it's a C array.
	    `(let* ((,num-elts ,nelts)
		    (,cffitype (lisptype->cffi ,element-type)))
	       (cond ((typep ,data 'cffi:pointer)
		      ,data)
		     ((typep ,data 'array)
		      (copy-into-foreign (ll-make-array ,num-elts ,cffi-type) 
					 ,data ,cffitype ,num-elts))
		     ((listp ,data)
		      ;; DATA is a list.  Use COERCE on all the elements
		      ;; to make sure that they have the right type.  If
		      ;; they don't, an error will be signaled.
		      (let ((,lisptype (cffitype->lisp ,element-type)))
			(loop for ,e in ,data 
			   for ,i from 0 to (1- ,num-elts) do
			     (setf (ll-aref ,a ,cffitype ,i) (coerce ,e ,lisptype))
			   finally (return ,a))))
		     (t (error "DATA must be a CFFI C pointer, Lisp array or Lisp list."))))))))



(defmethod initialize-instance :after ((x vec) &key)
  "Assuming that NELTS is correct, fix up MY-ARRAY so that it is in 
   the proper format, whether that be a Lisp array or C array.  Note
   that this sometimes requires copying and sometimes does not."
  (cond ((and (has-feature-p :use-lisp-arrays)
	      (typep (my-array x) cffi:pointer))
	 ;; We've got a pointer but we want a Lisp array. 
	 ;; Make one and copy in the C pointer.
	 (error "FIXME!"))
	((and (not (has-feature-p :use-lisp-arrays))
	      (typep (my-array x) array))
	 ;; We've got a Lisp array but we want a pointer.
	 ;; Make a new array and copy it in.
	 (let ((a (ll-make-array (nelts x) (element-type x))))
	   (setf (slot-value x 'my-array) 
		 (copy-into-foreign a (slot-value x 'my-array) 
				    (element-type x) (nelts x)))))
	(t nil))
  (call-next-method))
	   




(define-abstract-class matrix-like (data-container)
  (:documentation "Abstract base class for 2-D matrices and matrix views.")
  ((nrows :initarg :nrows :initform 0 :reader nrows
	  :documentation "Number of rows in the matrix (view)")
   (ncols :initarg :ncols :initform 0 :reader ncols
	  :documentation "Number of columns in the matrix (view)")
   (orientation :initarg :orientation
		:initform :column
		:reader orientation
		:documentation ":row or :column -- whether the underlying (1-D) storage of the 2-D matrix is in row-major or column-major order")
   ;;(storage :initarg :storage
   ;;    :initform :general
   ;;    :documentation ":general, :upper, :lower, (:banded lbw ubw)")
   ))





(defclass matrix (matrix-like)
  (:documentation "A (2-D) matrix.  Has ownership of the underlying
  data (unlike MATRIX-VIEW objects).  Depending upon how the specific
  Lisp implementation handles passing Lisp arrays to foreign
  functions, the underlying data may be either a Lisp 1-D array or a C
  pointer to a 1-D array.")) 

  
(defclass matview (matrix-like) 
  (:documentation "An abstract class representing a \"view\" into a
  matrix.  That view may be treated as a (readable and writeable)
  reference to the elements of the matrix.") 
  ((dim0 :initarg :dim0
	 :initform nrows
	 :reader dim0
	 :documentation "The first (row) dimension of MY-ARRAY.")
   (dim1 :initarg :dim1
	 :initform ncols
	 :reader dim1
	 :documentation "The second (column) dimension of MY-ARRAY.")))

(defclass vecview (vector-like))

(defclass strided-vecview (vecview)
  ((stride :initform 1
	   :initarg :stride
	   :reader stride)))

(defclass index-list-vecview (vecview)
  ((indices :initarg :indices
	    :reader indices)))

(defmethod initialize-instance :after ((A matview) &key)
  "Error checking for initialization of a MATVIEW object."
  (with-slots (nrows ncols dim0 dim1 orientation) A
    (assert (>= nrows 0))
    (assert (>= ncols 0))
    (assert (>= dim0 nrows))
    (assert (>= dim1 ncols))
    (assert (valid-orientation-p orientation))))


(defmethod matrix-dimension ((A matrix-like) which)
  "Like ARRAY-DIMENSION for matrix-like objects."
  (cond ((= which 0) (nrows A))
	((= which 1) (ncols A))
	(t (error "The given matrix(-view) has only two dimensions, but you tried to access dimension ~A" (1+ which)))))

(defmethod matrix-dimensions ((A matrix-like)) 
  "Like ARRAY-DIMENSIONS for matrix-like objects."
  (list (nrows A) (ncols A)))


;;;; WINDOW-MATVIEW
    
(defclass window-matview (matview)
  (:documentation
   "A WINDOW-MATVIEW views a block of elements in the underlying
   matrix that is conceptually 2-D contiguous.  If the underlying
   matrix is column-oriented, the elements in each column of a
   WINDOW-MATVIEW are stored contiguously, and horizontally adjacent
   elements are separated by a constant stride (\"LDA\" in BLAS
   terms).")
  ((offset0 :initarg :offset0
	    :initform 0
	    :reader offset0)
   (offset1 :initarg :offset1
	    :initform 0
	    :reader offset1)))

(defmethod window-subview ((A matrix) 
			   &key (:nrows (slot-value A 'nrows))
				(:ncols (slot-value A 'ncols))
				(:offset0 0)
				(:offset1 0))
  (make-instance 'window-matview 
    :my-array (slot-value A 'my-array)
    :nrows nrows
    :ncols ncols
    :dim0 (slot-value A 'nrows)
    :dim1 (slot-value A 'ncols)
    :offset0 offset0
    :offset1 offset1
    :orientation (slot-value A 'orientation)))

  
(defmethod window-subview ((A window-matview) 
			   &key (:nrows (slot-value A 'nrows))
				(:ncols (slot-value A 'ncols))
				(:offset0 0)
				(:offset1 0))
  "Creates a WINDOW-MATVIEW which is a \"subview\" of an existing WINDOW-MATVIEW."
  (make-instance 'window-matview 
    :my-array (slot-value A 'my-array)
    :nrows nrows
    :ncols ncols
    :dim0 (slot-value A 'dim0)
    :dim1 (slot-value A 'dim1)
    :offset0 (+ (slot-value A 'offset0) offset0)
    :offset1 (+ (slot-value A 'offset1) offset1)
    :orientation (slot-value A 'orientation)))


;;;; STRIDED-MATVIEW

(defclass strided-matview (matview)
  ((offset0 :initarg :offset0
	    :initform 0
	    :reader offset0)
   (offset1 :initarg :offset1
	    :initform 0
	    :reader offset1)
   (stride0 :initarg :stride0
	    :initform 1
	    :reader stride0)
   (stride1 :initarg :stride1
	    :initform 1
	    :reader stride1)))

(defmethod initialize-instance :after ((A strided-matview) &key)
  (with-slots (stride0 stride1 dim0 dim1) A
    ;; FIXME: negative strides are allowed; we need to check if they
    ;; are valid.
    (assert (< stride0 dim0))
    (assert (< stride1 dim1))))

(defmethod strided-subview ((A window-matview)
			    &key :nrows :ncols 
				 :offset0 :offset1 
				 :stride0 :stride1)
  (make-instance 'strided-matview 
    :my-array (slot-value A 'my-array)
    :nrows nrows
    :ncols ncols
    :dim0 (slot-value A 'dim0)
    :dim1 (slot-value A 'dim1)
    :offset0 (+ (slot-value A 'offset0) offset0)
    :offset1 (+ (slot-value A 'offset1) offset1)
    :stride0 stride0
    :stride1 stride1
    :orientation (slot-value A 'orientation)))

(defmethod strided-subview ((A strided-matview)
			    &key :nrows :ncols 
				 :offset0 :offset1 
				 :stride0 :stride1)
  ;; The specialized INITIALIZE-INSTANCE method takes care of error
  ;; checking.
  (make-instance 'strided-matview 
    :my-array (slot-value A 'my-array)
    :nrows nrows
    :ncols ncols
    :dim0 (slot-value A 'dim0)
    :dim1 (slot-value A 'dim1)
    :offset0 (+ (slot-value A 'offset0) offset0)
    :offset1 (+ (slot-value A 'offset1) offset1)
    :stride0 (* (slot-value A 'stride0) stride0)
    :stride1 (* (slot-value A 'stride1) stride1)
    :orientation (slot-value A 'orientation)))




;;;; MATRIX OPERATORS

;;;; STUFF WE NEED FOR AREF

(defmethod unit-stride-p ((A matrix))
  t)
  
(defmethod unit-stride-p ((A window-matview))
  t)

(defmethod unit-stride-p ((A strided-matview))
  (and (= 1 (slot-value A 'stride0))
       (= 1 (slot-value A 'stride1))))

(defgeneric matview-index-to-1d-index (A i j))

(defmethod matview-index-to-1d-index ((A window-matview) i j)
  (with-slots (offset0 offset1 orientation dim0 dim1) A
    (cond ((eql orientation :column)
	   (+ offset0 i (* dim0 (+ offset1 j))))
	  ((eql orientation :row)
	   (+ offset1 j (* dim1 (+ offset0 i))))
	  (t (error "Invalid orientation ~A" orientation)))))

(defmethod matview-index-to-1d-index ((A matrix) i j)
  (with-slots (orientation nrows ncols) A
    (cond ((eql orientation :column)
	   (+ i (* nrows j)))
	  ((eql orientation :row)
	   (+ j (* ncols i)))
	  (t (error "Invalid orientation ~A" orientation)))))

(defmethod matview-index-to-1d-index ((A strided-matview) i j)
  (with-slots (offset0 offset1 orientation
	       dim0 dim1 stride0 stride1) A
    (cond ((eql orientation :column)
	   (+ offset0 (* i stride0) (* dim0 (+ offset1 (* j stride1)))))
	  ((eql orientation :row)
	   (+ offset1 (* j stride1) (* dim1 (+ offset0 (* i stride0)))))
	  (t (error "Invalid orientation ~A" orientation)))))

(defmethod mref ((A matrix-like) i j)
  "MREF is the equivalent of AREF for MATRIX-LIKE objects (matrices 
   and matrix views).  This function involves a lot of index calculations.
   If you're in a loop over all the elements of an array, it may be better
   to unpack the data and handle the indexing manually.  You can also use
   series functions on the result of a SCAN-MATVIEW method, if you want to
   iterate over all the elements of a MATRIX-LIKE object."
  (aref (my-array A) (matview-to-1d-index A i j)))

(defmethod matrix-total-size ((A matrix-like))
  "Returns the total number of elements in the given MATRIX-LIKE object."
  (* (nrows A) (ncols A)))

(defmacro iterate-over-window-matview (A &body body)
  "Applies BODY to each element of the WINDOW-MATVIEW A. 
   In BODY, \"the-element\" captures the current element of the matrix."
  `(with-slots (my-array orientation nrows ncols dim0 dim1 offset0 offset1) ,A
     (assert (eql (type-of ,A) 'window-matview))
     (cond ((eql orientation :column)
	    (do ((j (+ (* offset1 dim0) offset0)
		    (+ j dim0)))
		((>= j (* (+ offset1 ncols) dim0)) nil)
	      (do ((i j (1+ i)))
		  ((>= i nrows) nil)
		(symbol-macrolet ((the-element `(cl:aref my-array i)))
		  @,body))))
	   ((eql orientation :row)
	    (do ((i (+ (* offset0 dim1) offset1)
		    (+ i dim1)))
		((>= i (* (+ offset0 nrows) dim1)) nil)
	      (do ((j i (1+ j)))
		  ((>= j ncols) nil)
		(symbol-macrolet ((the-element `(cl:aref my-array j)))
		  @,body))))
	   (t (error "Invalid orientation ~A" orientation)))))


(defgeneric scan-matview (A)
  (:documentation "Just like the SCAN function takes a sequence and returns the corresponding series, except that it takes a MATVIEW as input.  This generic function is specialized on the various subclasses of MATVIEW for efficient iteration over the elements of the MATVIEW -- that is, indexing overhead is amortized over the whole loop rather than recalculated for each array reference.  These functions may be useful for implementing elementwise operations that do not vary from element to element."))


(defmethod scan-matview ((A matview))
  "This is the default SCAN-MATVIEW method.  It uses the suboptimal
   MREF.  It also iterates over the elements in column-oriented fashion,
   which may not be what you want.  FIXME:  It should iterate according
   to the orientation of the matrix!"
  (with-slots (nrows ncols) A
    (cond ((= 0 (* nrows ncols))
	   #Z())
	  (t 
	   (producing (w) ((i 0)
			   (j 0))
		      (declare (type (series (type-of (mref A i j))) w))
		      (loop 
			(tagbody
			  (when (>= i nrows)
			    (incf j)
			    (setf i 0))
			  (when (>= j ncols)
			    (terminate-producing))
			  (next-out w (mref A i j))
			  (incf i))))))))

(defmethod scan-matview ((A window-matview))
  "For a WINDOW-MATVIEW, the scan direction follows the orientation
   of the matrix."
  (with-slots (my-array nrows ncols orientation dim0 dim1 offset0 offset1) A
    (cond ((= 0 (* nrows ncols))
	   #Z())
	  ((eql orientation :column)
	   (let ((colstart (+ (* offset1 dim0) offset0))
		 (colend (+ (* offset1 dim0) offset0 nrows)))
	     (producing (w) ((i colstart) (j 0))
			(loop 
			  (tagbody
			    (when (>= i colend)
			      (incf colstart dim0)
			      (incf colend dim0)
			      (incf j))
			    (when (>= j ncols)
			      (terminate-producing))
			    (next-out w (aref my-array i))
			    (incf i))))))
	  ((eql orientation :row)
	   (let ((rowstart (+ (* offset0 dim1) offset1))
		 (rowend (+ (* offset0 dim1) offset0 ncols)))
	     (producing (w) ((j rowstart) (i 0))
			(loop
			  (tagbody
			    (when (>= j rowend)
			      (incf rowstart dim1)
			      (incf rowend dim1)
			      (incf i))
			    (when (>= i nrows)
			      (terminate-producing))
			    (next-out w (aref my-array j))
			    (incf j))))))
	  (t (error "Invalid orientation ~A" orientation)))))

(defmethod scan-matview ((A matrix))
  "SCAN-MATVIEW for a MATRIX works just like for a WINDOW-MATVIEW."
  (scan-matview (window-subview A)))
	   
(defmethod scan-matview ((A strided-matview))
  (with-slots (my-array nrows ncols orientation dim0 dim1 
	       offset0 offset1 stride0 stride1) A
    (cond ((= 0 (* nrows ncols))
	   #Z())
	  ((eql orientation :column)
	   (let ((colstart (+ (* offset1 dim0) offset0))
		 (colend (+ (* offset1 dim0) offset0 (* nrows stride0))))
	     (producing (w) ((i colstart) (j 0))
			(loop 
			  (tagbody
			    (when (>= i colend)
			      (incf colstart dim0)
			      (incf colend dim0)
			      (incf j stride1))
			    (when (>= j ncols)
			      (terminate-producing))
			    (next-out w (aref my-array i))
			    (incf i stride0))))))
	  ((eql orientation :row)
	   (let ((rowstart (+ (* offset0 dim1) offset1))
		 (rowend (+ (* offset0 dim1) offset0 (* ncols stride1))))
	     (producing (w) ((j rowstart) (i 0))
			(loop
			  (tagbody
			    (when (>= j rowend)
			      (incf rowstart dim1)
			      (incf rowend dim1)
			      (incf i stride0))
			    (when (>= i nrows)
			      (terminate-producing))
			    (next-out w (aref my-array j))
			    (incf j stride1))))))
	  (t (error "Invalid orientation ~A" orientation)))))

(defmethod transpose ((A matrix))
  "The transpose of a MATRIX is by default a WINDOW-MATVIEW.
   If you want a copy, call the appropriate COPY function on 
   the return value of TRANSPOSE.  The reason for this is to
   avoid expensive operations whenever possible.  Many BLAS
   and LAPACK routines have a \"TRANSA\" argument (or similar)
   that lets you specify that the operation should work with 
   the transpose of the matrix.  This means that it usually 
   isn't necessary to compute an explicit transpose, at least
   for input arguments."
  (make-instance window-matview
    :my-array (slot-value A 'my-array)
    :nrows (slot-value A 'ncols)
    :ncols (slot-value A 'nrows)
    :orientation (opposite-orientation (slot-value A 'orientation))
    :dim0 (slot-value A 'ncols)
    :dim1 (slot-value A 'nrows)
    :offset0 0
    :offset1 0))
  
(defmethod transpose ((A window-matview))
  (make-instance window-matview
    :my-array (slot-value A 'my-array)
    :nrows (slot-value A 'ncols)
    :ncols (slot-value A 'nrows)
    :orientation (opposite-orientation (slot-value A 'orientation))
    :dim0 (slot-value A 'dim1)
    :dim1 (slot-value A 'dim0)
    :offset0 (slot-value A 'offset1)
    :offset1 (slot-value A 'offset0)))

(defmethod transpose ((A strided-matview))
  (make-instance window-matview
    :my-array (slot-value A 'my-array)
    :nrows (slot-value A 'ncols)
    :ncols (slot-value A 'nrows)
    :orientation (opposite-orientation (slot-value A 'orientation))
    :dim0 (slot-value A 'dim1)
    :dim1 (slot-value A 'dim0)
    :offset0 (slot-value A 'offset1)
    :offset1 (slot-value A 'offset0)
    :stride0 (slot-value A 'stride1)
    :stride1 (slot-value A 'stride0)))

(defmethod copy ((A matrix-like) &key (:orientation :column))
  "The COPY method always returns a MATRIX.  The default COPY 
   probably works as efficiently as Lisp can work; you may wish 
   to specialize COPY for specific data types to exploit machine 
   features (e.g. long registers and/or memory operations), 
   though remember that for large matrices, performance is 
   bandwidth-limited anyway.

   Implementation note:  for a COPY-INTO (destructive) routine,
   we would need to be able to scan over the original matrix and
   feed the results one-by-one into a vector.  It's probably too
   hard to do that with SERIES functions; we probably have to 
   specialize that for each matrix view type."
  (with-slots (my-array nrows ncols) A
    (cond ((eql (slot-value A 'orientation) orientation)
	   (make-instance 'matrix
	     :my-array (collect '(vector (type-of (mref A 0 0))) 
				(scan-matview A))
	     :nrows nrows
	     :ncols ncols
	     :orientation orientation))
	  (t 
	   (make-instance 'matrix
	     :my-array (collect '(vector (type-of (mref A 0 0)))
				(scan-matview (transpose A)))
	     :nrows nrows
	     :ncols ncols
	     :orientation orientation)))))

	
(defun mark-output-variables-helper (tree function-list variable-list output-p)
  (if (and (atom tree) output-p (member tree variable-list)) 
      ;; We've found an output argument which is one of our variables.  
      ;; Mark it as an output argument.
      (setf (get tree 'output-argument) t)
    ;; We've got structure.  Look for one of our function calls.
    (if (member (car tree) function-list)
	;; We've found one of our function calls.  Go through its 
	;; arguments and mark those that are output arguments.
	(loop 
	    for attrib in (get (car tree) 'parameter-attributes)
	    for arg in (cdr tree)
	    do
	      (if (eq attrib 'output)
		  (mark-output-variables-helper arg function-list variable-list t)))
      nil)))
(defun mark-output-variables (tree function-list variable-list)
  (mark-output-variables-helper tree function-list variable-list nil))
    
     




(defmethod restore ((OUT matview) (IN matrix)))
(defmethod restore ((OUT window-matview) (IN matrix)))
(defmethod restore ((OUT strided-matview) (IN matrix)))

  
  


;;; OPTIONAL-COPY and OPTIONAL-RESTORE copy and restore only if the
;;; argument to OPTIONAL-COPY is not a MATRIX or WINDOW-MATVIEW.
;;; Otherwise, OPTIONAL-COPY just returns the argument unchanged.
;;; OPTIONAL-RESTORE checks if the first argument is EQ to the second
;;; argument; if so, it does nothing, otherwise it restores.

(defmethod optional-copy ((A matrix-like))
  (copy A))
(defmethod optional-copy ((A matrix))
  A)
(defmethod optional-copy ((A window-matview))
  A)
(defmethod optional-copy ((A strided-matview))
  (copy A))
(defmethod optional-copy ((A vector-like))
  (copy A))
(defmethod optional-copy ((A vector))
  A)
(defmethod optional-copy ((A window-vecview))
  A)
;; Strided vectors can be handled easily enough by the INCX argument. 
(defmethod optional-copy ((A strided-vecview))
  A)


(defun optional-restore (OUT IN)
  (if (eq IN OUT)
      OUT
    (restore OUT IN)))

(defmacro with-copies (var-list &body body)
  (let* ((let-initialization-form 
	  ;; Creates the LET initialization form
	  (mapcar #'(lambda (x) `(optional-copy ,(gensym) ,x)) var-list))
	 ;; List of the gensyms we used above.
	 (gensym-list (mapcar #'(lambda (p) (cadr p)) let-initialization-form))
	 ;; Does the substitution in BODY for each LET initialization form.
	 (new-body (mapcar #'(lambda (p) (mapcar #'(lambda (L) 
						     (subst (cadr p) (caddr p) L))
						 body))
			   let-initialization-form)))
    ;; We have a list of functions about which we know.  Look for
    ;; each of these in turn in BODY.  We know that these functions
    ;; work in-place; they don't return a matrix or vector.  
    ;; This means we can examine their arguments and figure out
    ;; which GENSYMs are to be marked as output arguments.
    ;; 
    ;; a. Go through BODY and see if any of the functions are ours.
    ;;
    ;; b. If we find a function that's ours, check its arguments.
    ;;    The arguments may be more complex than just gensyms.
    ;;    For simplicity, we can assume that the existence of any
    ;;    gensym in an output argument position makes that gensym
    ;;    an output argument.  We assume that the user isn't doing
    ;;    foolish things like the following:
    ;;
    ;;    (gemm alpha A B beta (copy C))
    ;; 
    ;;    or the following:
    ;;
    ;;    (gemm alpha A B beta (setf D (copy C)))
    ;;
    ;; c. If we find that a gensym is an output argument, we add a 
    ;;    flag to its property list.
    ;; 
    ;; d. When we're done scanning the BODY, we can emit a list of 
    ;;    restoration (copy-back) forms using those gensyms that 
    ;;    are output arguments.
    (progn
      (mark-output-variables (tree +our-functions+ gensym-list))
      (let ((restoration-list 
	     (remove-if #'(lambda (L) (not (get (cadr L) 'output-argument)))
			(mapcar #'(lambda (p) `(optional-restore ,(car p) ,(cadr p)))
				let-initialization-form))))
	;; Emit the code.
	`(let `,let-initialization-form
	   (prog1
	       (progn
		 @,body)
	     @,restoration-list))))))
	   
  
;;; Alist of the CBLAS functions we support and their method templates.
;;; WRONG! -- fix for CBLAS.
(defconstant +our-functions+ 
    '((gemm . (defmethod gemm ((alpha --DATATYPE-- --IN--)
			       (A --MATRIX-- --IN--)
			       (B --MATRIX-- --IN--)
			       (beta --DATATYPE-- --IN--)
			       (C --MATRIX-- --INOUT--))
		(assert (= ncols A) (= nrows B))
		(with-copies --COPIES-LIST--
		  (--WRAPPER-- "N" "N" (nrows C) (ncols C) (ncols A)
			       alpha (my-array A) (dim0 A) (my-array B)
			       (dim0 B) beta (my-array C) (dim0 C)))))
      (dot . (defmethod dot ((alpha --DATATYPE-- --IN--)
			     (x --VECTOR-- --IN--)
			     (y --VECTOR-- --IN--))
	       (assert (= (nelts x) (nelts y)))
	       (let ((incx (if (typep x 'strided-vecview)
			       (stride x)
			     1))
		     (incy (if (typep x 'strided-vecview)
			       (stride x)
			     1)))
		 (with-copies --COPIES-LIST--
		   (--WRAPPER-- (nelts x) (my-array x) incx (my-array y) incy)))))))

;;; Datatypes that are supported by LAPACK and the BLAS.
(defconstant +supported-datatypes+ '(double-float (complex double-float)
				     single-float (complex single-float)))

(defun datatype->letter (datatype)
  "Converts the given DATATYPE to the letter that symbolizes 
   it in the BLAS and LAPACK."
  (cond ((eql datatype 'double-float) "d")
	((eql datatype 'single-float) "s")
	((eql datatype '(complex double-float) "z"))
	((eql datatype '(complex single-float) "c"))
	(t (error "LAPACK does not support the datatype ~A" datatype))))







(defun make-copy-list (arguments-template)
  "Generates the list of matrix-like arguments to feed into WITH-COPIES.  All arguments marked in the template as --MATRIX-- are included."
  (let ((copy-list '()))
    (dolist (arg arguments-template (reverse copy-list))
      (if (and (not (atom arg))
	       (eq (cadr arg) '--MATRIX--))
	  (setf copy-list (cons arg copy-list))))))

(defun make-argument-attributes-list (arguments-template)
  (let ((attribs-list '()))
    (dolist (arg arguments-template (reverse attribs-list))
      (if (and (not (atom arg))
	       (or (eq (car (cddr arg)) '--OUT--)
		   (eq (car (cddr arg)) '--INOUT--)))
	  (setf attribs-list (cons 'output attribs-list))
	(setf attribs-list (cons 'input attribs-list))))))

(defun method->wrapper (method-name datatype)
  "Given a method name and a datatype, returns the name of the 
   corresponding (C)BLAS routine.
   FIXME:  this only works for the CBLAS -- what about LAPACK?"
  (concatenate 'string "cblas_" (datatype->letter datatype) method-name))

(defun template->defmethod (template datatype)
  (let ((method-name (cadr template))
	(arguments (car (cddr template))))
    ;; SUBLIS does all the substitutions at once.  Pretty handy!
    (sublis `((--WRAPPER-- . ,(method->wrapper method-name datatype))
	      (--DATATYPE-- . ,datatype)
	      (--MATRIX-- . matrix-like)
	      (--COPY-LIST-- . ,(make-copy-list arguments)))
	    template)))


(defmacro generate-methods (template supported-datatypes))
  #|
  "Given a method template TEMPLATE and the list of supported datatypes,
   generates a DEFMETHOD for each supported datatype.
  (let ((method-name (cadr template))
	(arguments-template (car (cddr template)))
	(methods (loop for datatype in supported-datatypes
	               collect (template->defmethod template datatype))))
    `(progn 
       (setf (get ,method-name 'param-type) 
	 ',(make-argument-attributes-list arguments-template))
       @,methods)))
|#


(defmacro generate-all-methods (supported-functions supported-datatypes)
  `(progn @,(loop 
	       for p in supported-functions 
	       collect (let ((method-name (car p))
			     (method-template (cadr p)))
			 (generate-methods method-template 
					   supported-datatypes)))))

;;; Generate all the methods!
(generate-all-methods +our-functions+ +supported-datatypes+)

   
   
   
;;;; What does packed, e.g. symmetric, storage mean for a matview?
;;;;
;;;; 1. Coordinates (2-D) in a matview xlate to coordinates in a matrix (2-D)
;;;; 2. If coordinates are outside of stored range, supply the actual coords
;;;; 3. Should we support Hermetian, etc., storage?
