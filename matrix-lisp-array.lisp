(in-package :lisp-matrix)

;;;; * Matrices based on lisp simple-arrays of rank 1
;;;;
;;;; We implement here matrices based on lisp simple-arrays of rank 1.
;;;; The implementation will be named :LISP-ARRAY, and specific
;;;; functions that we introduce will have "LA" in their name.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-implementation :lisp-array "LA"))

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

;;;; Also, some lisps (e.g., CLISP) fill the matrix with NIL if we
;;;; don't provide INITIAL-ELEMENT or an INITIAL-CONTENTS, so for
;;;; those we add an :AROUND method to make sure that the element
;;;; satisfies the element-type.

#+clisp
(defmethod make-matrix* :around
    (nrows ncols (matrix-implementation (eql :lisp-array))
           &key element-type initial-element)
  (declare (ignore initial-element))
  (let ((matrix (call-next-method)))
    (unless (or (zerop nrows)
                (zerop ncols)
                (typep (mref matrix 0 0) element-type))
      (fill-matrix matrix (la-default-value element-type)))
    matrix))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *la-default-value-table* nil
    "Table of default element types."))

(defun la-default-value (element-type)
  "Default value for a given ELEMENT-TYPE."
  (cdr (assoc element-type *la-default-value-table* :test #'equal)))

(defun add-la-default-value (element-type value)
  "Add VALUE as default value for ELEMENT-TYPE."
  (pushnew (cons element-type value)
           *la-default-value-table* :test #'equal))

(defmethod fill-matrix ((matrix la-matrix) fill-element)
  (fill (data matrix) fill-element)
  matrix)

;;;; ** Typed matrices

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun la-matrix-class (element-type &optional (type :simple))
    "Return the LA-MATRIX class name corresponding to ELEMENT-TYPE."
    (matrix-class type :lisp-array element-type))
  
  (defmacro construct-la-matrix (element-type default-value)
    "Construct a matrix class holding elements of type ELEMENT-TYPE
    based on lisp arrays."
    (let* ((la-typed-class (la-matrix-class element-type :simple))
           (la-typed-base-class (la-matrix-class element-type :base)))
      `(progn

         (add-la-default-value ',element-type ,default-value)
         
         (make-class-hierarchy :lisp-array ,element-type)
         
         (defclass ,la-typed-class (,la-typed-base-class)
           ((data :initarg :data
                  :accessor data
                  :type (simple-array ,element-type (*))
                  :documentation "The lisp simple-array of rank 1
                  holding the elements."))
           (:documentation ,(format nil "Dense matrix holding ~
           elements of type ~A, implemented as a lisp array."
           element-type)))))))

(construct-la-matrix single-float 0.0)
(construct-la-matrix double-float 0d0)
(construct-la-matrix (complex single-float) #C(0.0 0.0))
(construct-la-matrix (complex double-float) #C(0d0 0d0))
(construct-la-matrix fixnum 0)
(construct-la-matrix integer 0)
(construct-la-matrix t nil)
