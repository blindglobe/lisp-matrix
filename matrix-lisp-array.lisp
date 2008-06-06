(in-package :lisp-matrix)

;;;; * Matrices based on lisp simple-arrays of rank 1
;;;;
;;;; We implement here matrices based on lisp simple-arrays of rank 1.
;;;; The implementation will be named :LISP-ARRAY, and specific
;;;; functions that we introduce will have "LA" in their name.

(defclass la-matrix (matrix-like) ())

(defmethod implementation ((matrix la-matrix))
  :lisp-array)

(defmethod mref ((matrix la-matrix) i j)
  (aref (data matrix) (flatten-matrix-indices matrix i j)))

(defmethod (setf mref) (value (matrix la-matrix) i j)
  (setf (aref (data matrix) (flatten-matrix-indices matrix i j))
        value))

(defmethod make-matrix* (nrows ncols
                        (matrix-implementation (eql :lisp-array))
                        &key element-type
                         (initial-element nil initial-element-p))
  (make-instance
   (la-matrix-class element-type)
   :nrows nrows
   :ncols ncols
   :data (apply #'make-array (* nrows ncols)
                :element-type element-type
                (when initial-element-p
                  (list :initial-element initial-element)))))

;;;; ** Typed matrices
;;;; 
;;;; For classes representing typed matrices, we will base the class
;;;; names on a name derived from the element type.  We also define
;;;; the names as lisp types, and define two functions to convert
;;;; between lisp types and names.

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *la-name-table* nil
   "Table associating LA-NAMEs to their corresponding lisp type.  These
  names are used in the class names of matrices subclassing LA-MATRIX,
  and the lisp type is the element-type of the matrices.")

 (defun element-type->la-name (element-type)
   "Return the LA-NAME corresponding to the lisp type ELEMENT-TYPE."
   (car (rassoc element-type *la-name-table* :test #'equal)))

 (defun la-name->element-type (la-name)
   "Return the lisp type corresponding to LA-NAME."
   (cdr (assoc la-name *la-name-table*)))

 (defun la-matrix-class (element-type &optional (type :simple))
   "Return the LA-MATRIX class name corresponding to ELEMENT-TYPE."
   (matrix-class type "LA" (element-type->la-name element-type))))

;;;; We now introduce a macro to construct the typed matrices.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro construct-la-matrix (la-type element-type)
    "Construct a matrix class holding elements of type ELEMENT-TYPE
    based on lisp arrays.  LA-TYPE is the name that will be used to
    identify the element-type."
    (pushnew (cons la-type element-type) *la-name-table*
             :test #'equal)
    (let* ((la-type-name
            (symbol-name (element-type->la-name element-type)))
           (la-typed-class (matrix-class :simple "LA" la-type-name))
           (la-typed-base-class (matrix-class :base "LA"
                                              la-type-name)))
      `(progn
         ,(unless (eql la-type element-type)
                  `(deftype ,la-type () ',element-type))

         (make-class-hierarchy :lisp-array "LA" ,element-type
                               ,la-type-name)
         
         (defclass ,la-typed-class (,la-typed-base-class)
           ((data :initarg :data
                  :accessor data
                  :type (simple-array ,element-type (*))
                  :documentation "The lisp simple-array of rank 1
                  holding the elements."))
           (:documentation ,(format nil "Dense matrix holding ~
           elements of type ~A, implemented as a lisp array."
                                    element-type)))))))

(construct-la-matrix double double-float)
(construct-la-matrix single single-float)
(construct-la-matrix complex-single (complex single-float))
(construct-la-matrix complex-double (complex double-float))
(construct-la-matrix fixnum fixnum)
(construct-la-matrix integer integer)
(construct-la-matrix t t)
