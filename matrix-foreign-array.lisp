(in-package :lisp-matrix)

;;;; * Matrices based on foreign arrays
;;;;
;;;; We implement here matrices based foreign arrays, implemented
;;;; through the foreign-numeric-vector (FNV) library. The
;;;; implementation will be named :FOREIGN-ARRAY, and specific
;;;; functions that we introduce will have "FA" in their name.

(defclass fa-matrix (matrix-like) ())

(defmethod implementation ((matrix fa-matrix))
  :foreign-array)

(defmethod make-matrix* (nrows ncols
                         (matrix-implementation (eql :foreign-array))
                         &key element-type initial-element)
  (make-fa-matrix nrows ncols (element-type->fnv-type element-type)
                  :initial-element initial-element))

(defgeneric make-fa-matrix (nrows ncols fnv-type
                                       &key initial-element)
  (:documentation "Same as MAKE-MATRIX*, but specific to matrix of
  implementation :FOREIGN-ARRAY and specialize on FNV-TYPE."))

;;;; ** Typed matrices
;;;;
;;;; For classes representing typed matrices, we will base the class
;;;; names on the name used in the FNV library.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *fnv-type-table*
    '((float . single-float)
      (double . double-float)
      (complex-float . (complex single-float))
      (complex-double . (complex double-float))))

  (defun fnv-type->element-type (fnv-type)
    "Return the lisp type corresponding to FNV-TYPE."
    (cdr (assoc fnv-type *fnv-type-table*)))

  (defun element-type->fnv-type (element-type)
    "Return the FNV type corresponding to ELEMENT-TYPE."
    (car (rassoc element-type *fnv-type-table* :test #'equal))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro construct-fa-matrix (fnv-type)
    (let* ((element-type (fnv-type->element-type fnv-type))
           (fnv-type-name (symbol-name fnv-type))
           (fa-typed-class (matrix-class :simple "FA" fnv-type-name))
           (fa-typed-base-class (matrix-class :base "FA"
                                              fnv-type-name))
           (fnv-class (make-symbol* "FNV-" fnv-type))
           (fnv-ref (make-symbol* "FNV-" fnv-type "-REF"))
           (make-fnv (make-symbol* "MAKE-FNV-" fnv-type)))
      `(progn
         
         (make-class-hierarchy :foreign-array "FA" ,element-type
                               ,(symbol-name fnv-type))
         
         (defclass ,fa-typed-class (,fa-typed-base-class)
           ((data :initarg :data
                  :accessor data
                  :type ,fnv-class
                  :documentation "The FNV object holding the
                  elements."))
           (:documentation ,(format nil "Dense matrix holding ~
            elements of type ~A, implemented as a foreign array."
                                    element-type)))
         
         (defmethod mref ((matrix ,fa-typed-class) i j)
           (,fnv-ref (data matrix)
                     (flatten-matrix-indices matrix i j)))

         (defmethod (setf mref) (value (matrix ,fa-typed-class) i j)
           (setf (,fnv-ref (data matrix)
                           (flatten-matrix-indices matrix i j))
                 value))
         
         (defmethod make-fa-matrix (nrows ncols
                                    (fnv-type (eql ',fnv-type))
                                    &key initial-element)
           (let ((data (,make-fnv (* nrows ncols)
                                  :initial-value initial-element)))
             (make-instance ',fa-typed-class :nrows nrows :ncols ncols
                            :data data)))))))

(construct-fa-matrix double)
(construct-fa-matrix float)
(construct-fa-matrix complex-double)
(construct-fa-matrix complex-float)
