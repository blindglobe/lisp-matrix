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

;;;; ** Typed matrices

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun la-matrix-class (element-type &optional (type :simple))
    "Return the LA-MATRIX class name corresponding to ELEMENT-TYPE."
    (matrix-class type :lisp-array element-type))
  
  (defmacro construct-la-matrix (element-type)
    "Construct a matrix class holding elements of type ELEMENT-TYPE
    based on lisp arrays."
    (let* ((la-typed-class (la-matrix-class element-type :simple))
           (la-typed-base-class (la-matrix-class element-type :base)))
      `(progn

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

(construct-la-matrix double-float)
(construct-la-matrix single-float)
(construct-la-matrix (complex single-float))
(construct-la-matrix (complex double-float))
(construct-la-matrix fixnum)
(construct-la-matrix integer)
(construct-la-matrix t)
