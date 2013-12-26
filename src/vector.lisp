(in-package :lisp-matrix)

;;;; * Vectors
;;;;
;;;; Vector can be viewed as matrices that happen to have one row (or
;;;; one column), or as a separate type.
;;;;
;;;
;;; Tony comments: 1-d vectors are weird -- matrices seem to cover
;;; everything, including column vectors, row vectors, and scalars.
;;; But we've got some overheard if we let them handle everything.
;;; BUT, I think we can afford this -- how many times do we do large
;;; scale scalar arithmetic that can't be vectorized somehow to make
;;; it faster/better?  However, Luke/Ross brought up some good
;;; arguments for why we care about scalar computation -- it helps
;;; clarify a few issues from time to time.
;;; 
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
  views. Subclasses from the matrix-like superclass"))

(defgeneric vector-dimension (vector)
  (:documentation "Like ARRAY-DIMENSION for vector-like objects.  One
  can also used ARRAY-DIMENSION as well")
  (:method ((vector vector-like))
    (nelts vector)))

;; This API should be changed -- we are using type to denote data
;; type, so we should use ORIENTATION to denote row/column-ness.
(defgeneric vector-orientation (vector)
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

;;; Do we want to add the xref concept of possible read-only data
;;; structures for views, in order to do some form of protection?
;;; This would save space, with the penalty of checking for every setf
;;; operation to prevent inappropriate nuking.

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


;;; *** Diagonal view implemented via classes.
;;;
;;; Open question: should we restrict this to square matrices?  The
;;; operation of a diagonal is defined for rectangular matrices, but
;;; may not make so much sense (though some of the NxM decompositions
;;; use diagonals of subcomponents).   Currently, we assume this
;;; class/operation is good for square matrices

;;; need to dispatch properly when we have a transpose...!  (i.e. when
;;; we make inherited a transposed matrix, need to do the right
;;; thing...


;;; alternatively, make a vector creation function from a matrix,
;;; rather than a whole class -- this might be cleaner.
(defclass diagonal-vecview (vecview) ())

(defmethod vref ((vec diagonal-vecview) i)
  (mref (parent vec) i i))

(defmethod (setf vref) (value (vec diagonal-vecview) i)
  (setf (mref (parent vec) i i) value))

;; needed so that  m= works properly
(defmethod mref ((mat diagonal-vecview) i j)
  (ecase (vector-orientation mat)
    (:row
     (assert (zerop i))
     (vref mat j))
    (:column
     (assert (zerop j))
     (vref mat i))))

;;; FIXME: write!
(defgeneric diagonal! (mat &key type)
  (:documentation "create a vector representing the diagonal of matrix
  x.  This is a deep copy, NOT a view.  It might be easy to write the
  view as a class with appropriate 
    vref x i 
  pulling out 
    mref x i i 
  should we write such a view?")
  ;; (:method ((mat transpose-matview) &key (type :row)) )
  ;; (:method ((mat window-matview) &key (type :row)) )
  ;; (:method ((mat strided-matview) &key (type :row)) )
  (:method ((mat matrix-like) &key (type :row))
    (let ((nelts (min (ncols mat)
		      (nrows mat)))
	  ;; (stride (real-stride mat))
	  ;; (offset ;;??? )
	  )
      (make-instance 'diagonal-vecview
		     :parent mat
		     :nrows (ecase type (:row 1) (:column nelts))
		     :ncols (ecase type (:row nelts) (:column 1))
		     ;; :offset (offset mat)
		     ;; :stride stride
		     ))))
;; use class or make a copy and pull out as a function?


(defun diagonalf (mat &key (type :row))
  "This function provides a deep copy alternative to the diagonal
  class structure, and also might be cleaner to work with.  But might
  not be quite so flexible."
  (let* ((nelts (min (ncols mat)
		     (nrows mat)))
	 ;; (stride (real-stride mat))
	 ;; (offset ;;??? )
	 (d (make-vector nelts :type type)))
      (dotimes (i nelts)
	(setf (vref d i) (mref mat i i)))
      d))


;;;; *** Sliced View


(defclass slice-vecview (vecview)
  ((offset :initarg :offset
           :reader offset
           :initform 0)
   (stride :initarg :stride
           :reader stride
           :initform 1)))

(defgeneric real-stride (vector)
  (:documentation "Return the stride that VECTOR has when considering
  the ancestor matrix as base.  If VECTOR is constructed so that it
  has no simple stride (for example a slice on a windowed matrix
  considered as a vector may have a different stride when jumping
  columns of the matrix), then return NIL.")
  (:method ((vector matrix-like)) 1)
  (:method ((vector matview))
    "For MATVIEW vectors, in the general case I don't know how to
    compute the real stride."
    nil)
  (:method ((vector transpose-matview)) (real-stride (parent vector)))
  (:method ((vector slice-vecview))
    (let ((parent-stride (real-stride (parent vector))))
      (when parent-stride
        (* parent-stride (stride vector))))))

(defmethod zero-offset-p ((matrix slice-vecview))
  (and (= 0 (offset matrix))
       (zero-offset-p (parent matrix))))

(defmethod unit-strides-p ((matrix slice-vecview))
  (and (unit-strides-p (parent matrix))
       (etypecase (parent matrix)
         (vector-like t)
         ;; We have to reconstruct the corresponding row-stride and
         ;; col-stride of this view -- do this by inverting the code
         ;; in the function STRIDES for the case of 1 row or 1 column
         ;; 
         ;; FIXME: need to take into account orientation of MATRIX
         ;; (and its parent?)
         (matrix-like
          (ecase (vector-orientation matrix)
            (:row (= 1 (/ (stride matrix) (nrows (parent matrix)))))
            (:column (= 1 (stride matrix))))))))

;; FIXME: ugly
(defmethod mref ((matrix slice-vecview) i j)
  (ecase (vector-orientation matrix)
    (:row
     (assert (zerop i))
     (vref matrix j))
    (:column
     (assert (zerop j))
     (vref matrix i))))

(defmethod vref ((vector slice-vecview) i)
  (vref (parent vector)
        (+ (offset vector) (* i (stride vector)))))

(defmethod (setf vref) (value (vector slice-vecview) i)
  (setf (vref (parent vector)
              (+ (offset vector) (* i (stride vector))))
        value))

;; FIXME: should not be here
;; Tony asks:  why not? (need to describe this better sooner).
(defmethod vref ((matrix matview) i)
  (mref matrix (rem i (nrows matrix)) (truncate i (nrows matrix))))


;;;; ** Creating vectors
;;;;
;;;; Vectors are automatically created by matrix creation methods when
;;;; one of the dimensions is 1 (one), but we can also create them
;;;; explicitly by MAKE-VECTOR.

(defun make-vector (nelts &key (type :row)
                    (implementation *default-implementation*)
                    (element-type *default-element-type*)
                    (initial-element nil initial-element-p)
                    (initial-contents nil initial-contents-p))
  "Make a vector containing NELTS elements of type ELEMENT-TYPE, and
  with IMPLEMENTATION as underlying implementation.  The vector is a
  row vector if TYPE is :ROW, and a column vector if TYPE is :COLUMN.

  If INITIAL-ELEMENT is not specified, the vector is not initialized,
  and accessing its elements will thus return spurious values.

  If INITIAL-CONTENTS is specified, it is used to initialize the
  vector, by using the generic function COPY!.

  IMPLEMENTATION can be one of :LISP-ARRAY and :FOREIGN-ARRAY"
  (apply #'make-matrix
	 (ecase type (:row 1) (:column nelts)) ; nrows
         (ecase type (:row nelts) (:column 1)) ; ncols 
         :implementation implementation
         :element-type element-type
         (append (when initial-element-p
                   (list :initial-element initial-element))
                 (when initial-contents-p
                   (list :initial-contents initial-contents)))))

;;;; *** Vector views

(defmethod transpose-class ((matrix vector-like)) 'transpose-vecview)

(defgeneric slice-class (matrix)
  (:documentation "Return the name of the class to be used for a slice
  of MATRIX.")
  (:method ((matrix matrix-like)) 'slice-vecview))

(defgeneric slice (matrix &key offset stride nelts type)
  (:documentation "Create a slice view of MATRIX.  To be precise, this
   results in a vector (matrix subclass) which is done by
      :type   : output shape (:column or :row)
      :offset : number of observations (in col/row major
                matrix-dependent order) to skip over before starting
                extraction
      :stride : index increment between current and next element, i.e. 
                0 = repeat same value; 1, as ordered, 2 every other, 
                etc...  
   one of the challenges is that this seems to prefer col-oriented
   matrices, but we need to know whether we are column- or row-
   oriented. Perhaps we should be checking so to decide how to walk
   down the matrix?")
  (:method (matrix &key (offset 0) (stride 1)
	    (nelts (nelts matrix)) (type :row))
    (make-instance (slice-class matrix)
                   :parent matrix
                   :nrows (ecase type (:row 1) (:column nelts))
                   :ncols (ecase type (:row nelts) (:column 1))
                   :offset offset
                   :stride stride)))

(defmethod slice ((matrix slice-vecview) &key (offset 0) (stride 1)
                  (nelts (nelts matrix)) (type :row))
  "If MATRIX is a SLICE-VECVIEW, we can directly slice its parents by
  slightly modifying the parameters."
  (slice (parent matrix)
         :offset (+ offset (offset matrix))
         :stride (* stride (stride matrix))
         :nelts nelts
         :type type))
  
(defmethod slice ((matrix transpose-matview) &key (offset 0) (stride 1)
                  (nelts (nelts matrix)) (type :row))
  "For transposed matrices, the storage is the same, so we can slice
  its parent with the same arguments."
  (slice (parent matrix)
         :offset offset
         :stride stride
         :nelts nelts
         :type type))

(defgeneric row (matrix i)
  (:documentation "Return a view on a given row of MATRIX.")
  (:method ((matrix matrix-like) (i integer))
    (assert (< -1 i (nrows matrix)))
    (ecase (orientation matrix)
      (:column (slice matrix ;; verified
                      :offset i 
                      :stride (nrows matrix)
                      :nelts (ncols matrix)
                      :type :row))
      (:row (slice matrix
                   :offset  i
                   :stride 1
                   :nelts (ncols matrix)
                   :type :row))))
  (:method ((matrix transpose-matview) (i integer))
    (transpose-matrix (col (parent matrix) i)))
  (:method ((matrix window-matview) (i integer))
    (assert (< -1 i (nrows matrix)))
    (ecase (orientation matrix)
      (:column (slice (parent matrix)
                      :offset (+ (offset matrix) i)
                      :stride (nrows (parent matrix))
                      :nelts (ncols matrix)
                      :type :row))
      (:row (slice (parent matrix)
                   :offset (+ (offset matrix)
                              (* i (ncols (parent matrix))))
                   :stride 1
                   :nelts (ncols matrix)
                   :type :row))))
  (:method ((matrix strided-matview) (i integer))
    (assert (< -1 i (nrows matrix)))
    (ecase (orientation matrix)
      (:column (slice (parent matrix)
                      :offset (+ (offset matrix)
                                 (* i (row-stride matrix)))
                      :stride (* (nrows (parent matrix))
                                 (col-stride matrix))
                      :nelts (ncols matrix)
                      :type :row))
      (:row (slice (parent matrix)
                   :offset (+ (offset matrix)
                              (* i (ncols (parent matrix))))
                   :stride (row-stride matrix)
                   :nelts (ncols matrix)
                   :type :row)))))

;; TODO: similar to ROW.   See broken methods
(defgeneric col (matrix j)
  (:documentation "Return a view on a given column of MATRIX.")
  (:method ((matrix transpose-matview) (j integer))
    (transpose-matrix (row (parent matrix) j)))
  (:method ((matrix matrix-like) (j integer))
    (assert (< -1 j (ncols matrix)))
    (ecase (orientation matrix)
      (:column (slice matrix
                      :offset (* j (nrows matrix))
                      :stride 1
                      :nelts (nrows matrix)
                      :type :column))
      (:row (slice matrix
                   :offset (* j (ncols matrix)) 
                   :stride (ncols matrix)
                   :nelts (nrows matrix)
                   :type :column))))
  (:method ((matrix window-matview) (j integer))
    (assert (< -1 j (ncols matrix)))
    (ecase (orientation matrix)
      (:column (slice (parent matrix) ;; works, tested
                      :offset (+ (offset matrix)
			 (* j (nrows (parent matrix))))
                      :stride 1
                      :nelts (nrows matrix)
                      :type :column))
      (:row (slice (parent matrix) ;; think it works, not tested
                   :offset (+ (offset matrix) j)
                   :stride (ncols (parent matrix))
                   :nelts (nrows matrix)
                   :type :column))))
  (:method ((matrix strided-matview) (j integer))
    (assert (< -1 j (ncols matrix)))
    (ecase (orientation matrix)
      ;;   11 12 13 14 15 16
      ;;   21 22 23 24 25 26
      ;;   31 32 33 34 35 36
      ;;   41 42 43 44 45 46
      ;;   51 52 53 54 55 56
      ;; strided to:
      ;;   22 23 24
      ;;   42 43 44
      ;; by
      ;; (strides m01 :nrows 2 :ncols 3 :row-stride 2 :row-offset 1 :col-offset 1)
      ;; or:
      ;;
      ;;   xx xx xx xx xx xx
      ;;   xx 22 23 24 xx xx
      ;;   xx xx xx xx xx xx
      ;;   xx 42 43 44 xx xx
      ;;   xx xx xx xx xx xx
      ;;
      ;; i.e. need to balance with the nrows/ncols in the formula, as
      ;; well as strides/offsets.  Right now, THIS IS BROKEN.
      ;;
      ;;
      (:column (slice (parent matrix)
		      :offset (+ (offset matrix)
				 (* j (nrows (parent matrix))))
		      :stride  (row-stride matrix) ;; correct, I think!
                      :nelts (nrows matrix)
                      :type :column))
  ;;; FIXME THE REST OF THESE METHODS
      (:row (slice (parent matrix)
		   :offset (+ (offset matrix)
			      (* j (col-stride matrix)))
		   :stride (* (ncols (parent matrix))
			      (col-stride matrix))
                   :nelts (nrows matrix)
                   :type :column)))))




(defgeneric v= (x y)
  (:documentation "Test for equality of both number of elements and
  of the elements themselves, for the two vectors X and Y.  

  A row vector and a column vector with the same number of elements
  are equal.  To distinguish them, use M= instead.")
  (:method ((a vector-like) (b vector-like))
    (and (= (nelts a) (nelts b))
         (dotimes (i (nelts a) t)
           (unless (= (vref a i) (vref b i))
             (return-from v= nil))))))


(defgeneric v=2 (&rest args)
  (:documentation "Test for equality of both number of elements and
  of the elements themselves, for the two vectors X and Y.  

  A row vector and a column vector with the same number of elements
  are equal.  To distinguish them, use M= instead.")
  (:method (&rest args)
    (reduce #'(lambda (x y) (and x y))  ;; rewrite to use no args?
	    (loop while (and (typep 'vector-like (first  args))
			     (typep 'vector-like (second args)))
	       collect (v= (first args) (second args))))))
          


(defmethod print-object ((object vector-like) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d x ~d)" (nrows object) (ncols object))
    (dotimes (i (nelts object))
      (when (col-vector-p object)
        (terpri stream))
      (write-char #\space stream)
      (write (vref object i) :stream stream))))


#|

;;; Need copy methods for vectors?


(defmethod copy! ((a vector-like) (b vector-like))
  (unless (eq a b) ;; don't worry about same objects
    ;; FIXME: care about fast copy once everything is working
    (assert (= (nelts a) (nelts b)))
    (assert (subtypep (element-type a) (element-type b)))
    (dotimes (i (nelts a))
      (setf (vref b i) (vref a i))))
  b)


(defmethod copy! ((a vector-like) (b vector))
  (unless (eq a b) ;; don't worry about same objects
    ;; FIXME: care about fast copy once everything is working
    (assert (= (nelts a) (length b)))
    ;; FIXME: is the following possible?
    (assert (subtypep (element-type a) (element-type b)))
    (dotimes (i (nelts a))
      (setf (aref b i) (vref a i)))) ;; aref for vectors, but for lists? 
  b)


;;; List flattening
;;; needed for counting.  We don't export this, but would be nice to
;;; figure out a place to import this from.
;;; 
;;; '((1 2 3) (4 5) 6 7 (8)) into '(1 2 3 4 5 6 7 8)
(defun flatten-list (lst)
  "Flattens a list of lists into a single list.  Only useful when
we've mucked up data.  Sign of usage means poor coding!"
  (cond ((null lst) ;; endp?
	 nil)
        ((listp lst)
         (append (flatten-list (car lst)) (flatten-list (cdr lst))))
        (t
	 (list lst))))
;; (length (flatten-list (list 1 (list 2 3) 4  (list 5 6 7 (list 8 9 )))))
;; (length (flatten-list '(1 (2 3) 4 (5 6 7 (8 9)))))
;; (length (flatten-list (list 1 2 3)))

(defun nelts-list-as-v/m-like (x)
  (length (flatten-list x)))

(defmethod nelts ((x list))
  (nelts-list-as-v/m-like x))

;; (defparameter *x* (list 1 2 3))
;; (nth 1 *x*)
;; (nth 3 *x*)
;; (setf (nth 3 *x*) 4)

(defmethod copy! ((a vector-like) (b list))
  (let ((flat-b (flatten-list b)))
    (unless (eq a b) ;; don't worry about same objects
      ;; FIXME: care about fast copy once everything is working
      (assert (= (nelts a) (nelts flat-b)))
      ;; FIXME: not needed, since list isn't a fixed-type, and we are
      ;; coming from  fixed type structure!.
      ;; (assert (subtypep (element-type a) (element-type flat-b)))
      (dotimes (i (nelts a))
	(setf (nth i flat-b) (vref a i)))) ;; aref for vectors, but for lists? 
    flat-b))

(defmethod copy! ((a list) (b vector-like))
  (let ((flat-a (flatten-list a))
	(element-type (element-type b)))
    (unless (eq a b) ;; don't worry about same objects
      ;; FIXME: care about fast copy once everything is working
      (assert (= (nelts a) (nelts b)))
      (dotimes (i (nelts a))
	(assert (typep (nth i a) element-type ))
	(setf (vref b i) (nth i a))))
    b))

(defmethod copy! ((a vector) (b vector-like))
  (unless (eq a b) ;; don't worry about same objects
    ;; FIXME: care about fast copy once everything is working
    (assert (= (length a) (nelts b)))
    (let ((element-type (element-type b)))
      (dotimes (i (nelts a))
	(assert (typep (aref a i) element-type))
	(setf (vref b i) (aref a i)))))
  b)

(defmethod copy! ((a vector-like) (b vector-like))
  (unless (eq a b) ;; don't worry about same objects
    ;; FIXME: care about fast copy once everything is working
    (assert (= (nelts a) (nelts b)))
    ;; FIXME: is the following possible?
    (assert (subtypep (element-type a) (element-type b)))
    (dotimes (i (nelts a))
      (setf (vref b i) (vref a i)))) ;; aref for vectors, but for lists? 
  b)

|#

;;; Vectorized ops

(defgeneric v+ (x y &optional return-type)
  (:documentation "add a vector-like, vector, or list and return in a
  vector-like structure unless return-type is something else")
  (:method ((x vector-like)
	    (y vector-like)
	    &optional 
	    ;; ((return-type 'vector-like) symbol)
	    ;; (return-type symbol)
	    return-type)
    (assert (= (vector-dimension x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (+ (vref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y vector)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (+ (vref x i)
				 (aref y i))))
      result))
  (:method ((x vector) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (+ (aref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y list)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (+ (vref x i)
				 (nth i y))))
      result))
  (:method ((x list) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (+ (nth i x)
				 (vref y i))))
      result)))



(defgeneric v- (x y &optional return-type)
  (:documentation "add a vector-like, vector, or list and return in a
  vector-like structure unless return-type is something else")
  (:method ((x vector-like)
	    (y vector-like)
	    &optional 
	    ;; ((return-type 'vector-like) symbol)
	    ;; (return-type symbol)
	    return-type)
    (assert (= (vector-dimension x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (- (vref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y vector)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (- (vref x i)
				 (aref y i))))
      result))
  (:method ((x vector) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (- (aref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y list)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (- (vref x i)
				 (nth i y))))
      result))
  (:method ((x list) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (- (nth i x)
				 (vref y i))))
      result)))


(defgeneric v* (x y &optional return-type)
  (:documentation "add a vector-like, vector, or list and return in a
  vector-like structure unless return-type is something else")
  (:method ((x vector-like)
	    (y vector-like)
	    &optional 
	    ;; ((return-type 'vector-like) symbol)
	    ;;   (return-type symbol)
	    return-type)
    (assert (= (vector-dimension x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (* (vref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y vector)
	    &optional return-type)
    (assert (= (vector-dimension x) (array-dimension y 0)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (* (vref x i)
				 (aref y i))))
      result))
  (:method ((x vector) (y vector-like)
	    &optional return-type)
    (assert (= (array-dimension x 0) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (* (aref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y list)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (* (vref x i)
				 (nth i y))))
      result))
  (:method ((x list) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (* (nth i x)
				 (vref y i))))
      result)))



(defgeneric v/ (x y &optional return-type)
  (:documentation "add a vector-like, vector, or list and return in a
  vector-like structure unless return-type is something else")
  (:method ((x vector-like)
	    (y vector-like)
	    &optional 
	    return-type)
    (assert (= (vector-dimension x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (/ (vref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y vector)
	    &optional return-type)
    (assert (= (vector-dimension x) (array-dimension y 0)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (/ (vref x i)
				 (aref y i))))
      result))
  (:method ((x vector) (y vector-like)
	    &optional return-type)
    (assert (= (array-dimension x 0) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (/ (aref x i)
				 (vref y i))))
      result))
  (:method ((x vector-like) (y list)
	    &optional return-type)
    (assert (= (vector-dimension x) (length y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (vector-dimension x))))
      (dotimes (i (vector-dimension x))
	(setf (vref result i) (/ (vref x i)
				 (nth i y))))
      result))
  (:method ((x list) (y vector-like)
	    &optional return-type)
    (assert (= (length x) (vector-dimension y)))
    (unless return-type
      (setf return-type 'vector-like))
    (let ((result (make-vector (length x))))
      (dotimes (i (length x))
	(setf (vref result i) (/ (nth i x)
				 (vref y i))))
      result)))

#|
 (defmacro make-vectorized-generic-function (op)

  (defgeneric v`op (x y &optional return-type)
    (:documentation "add a vector-like, vector, or list and return in a
  vector-like structure unless return-type is something else")
    (:method ((x vector-like)
	      (y vector-like)
	      &optional 
	      ;;     (( return-type 'vector-like) symbol)
	      return-type)
      (assert (= (length x) (length y)))
      (let ((result (make-vector (length x))))
	(dotimes (i (length x))
	(setf (vref result i) (op (vref x i)
				  (vref y i))))
	result))
    (:method ( (x vector) (y vector-like)))
    (:method ( (x vector-like) (y vector)))
    (:method ( (x list) (y vector-like)))
    (:method ( (x vector-like) (y list)))
  ))
|#



#|
 (progn ;; vectorized arithmetic

  ;; So,  how do I vectorize something like:
  ;;     (a + b) / c  
  ;; (i.e. standard normalization) when a,b,c are vectors which have
  ;; the correct pre-computed values?

  ;; or...?  where the v.# operators disregard row vs. column oriented
  ;; aspect, and the v# operators worry about orientation.    So if we
  ;; know what we've got, we would then be able to do something like 

  ;;     (v/ (v+ a b) c)

  ;; or possibly

  ;;     (v/ (m+ a b) c)  ;; FIXME!

  ;; but we still need to figure out the API for vector ops, and whether
  ;; any of this is done by BLAS (which it should be) or LAPACK.
  
  ;; On a related note, we also could have m.# instead of v.# if
  ;; orientation needs to be ensured (rather than ignored).
  (defparameter *v1* (make-vector 4
				  :type :row
				  :initial-contents '((1d0 2d0 3d0 4d0))))
  (defparameter *v2* (make-vector 4
				  :type :row
				  :initial-contents '((10d0 20d0 30d0 40d0))))


  (defparameter *v1a* (make-vector 4
				  :type :column
				  :initial-contents '((1d0)(2d0)( 3d0 )(4d0))))
  (defparameter *v2a* (make-vector 4
				  :type :column
				  :initial-contents '((10d0)( 20d0)( 30d0)( 40d0))))
  (vector-dimension *v1*)
  (v=  (v+ *v1* *v2*)
       (v+ *v1a* *v2a*))
  (v=  (v+ *v1a* *v2*)
       (v+ *v1* *v2a*))

  (v- *v1* *v2*)
  (v- *v2* *v1*)
  (v* *v1* *v2*)
  (v/ *v1* *v2*)
  (v/ *v2* *v1*)

  (let* ((a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
	 (b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
	 (c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0)))))
    (v= (v+ a b)
	c)
    (v= (v+ b a)
	c))

  (defparameter a (make-vector 4 :initial-contents '((1d0 2d0 3d0 4d0))))
  (defparameter b (make-vector 4 :initial-contents '((10d0 20d0 30d0 40d0))))
  (defparameter c (make-vector 4 :initial-contents '((11d0 22d0 33d0 44d0))))
  (v= (v+ a b)
      c)
  (v= (v+ b c)
      c)

  (princ "vector ops done."))
|#


(defun map-vec (unifcn vec)
  "Apply univariate function to each element of old vector, returning
new one.
 (map-vec #'(lambda (x) (* 2.0 x))
          (make-vector 4 :initial-element 2d0))
 => #<LA-SIMPLE-VECTOR-DOUBLE (1 x 4) 4.0 4.0 4.0 4.0>
 (map-vec #'sqrt
          (make-vector 4 :initial-element 2d0))
 => #<LA-SIMPLE-VECTOR-DOUBLE (1 x 4) 1.4142135623730951 1.4142135623730951 1.4142135623730951 1.4142135623730951>"
  (check-type vec vector-like)
  (let ((tempvec (make-vector (nelts vec) :initial-element 0d0)))
    (dotimes (i (nelts vec))
      (setf (vref tempvec i)  (funcall unifcn (vref vec i)) ))
    tempvec))
