(in-package :lisp-matrix)

;;;; * Matrices based on foreign arrays
;;;;
;;;; We implement here matrices based foreign arrays, implemented
;;;; through the foreign-numeric-vector (FNV) library. The
;;;; implementation will be named :FOREIGN-ARRAY, and specific
;;;; functions that we introduce will have "FA" in their name.

;;; Tony sez: We need to describe that these are usually in
;;; row-orientation (or column orientation?  What do I know??  Why
;;; isn't it clear?  Why havn't I doc'd it :-)?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-implementation :foreign-array "FA"))

(defclass fa-matrix (matrix-like) ())

(defclass fa-vector (vector-like fa-matrix) ())

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
    (car (rassoc element-type *fnv-type-table* :test #'equal)))
  
  (defmacro construct-fa-matrix (element-type)
    (let* ((fnv-type (element-type->fnv-type element-type))
           (fa-typed-mclass (matrix-class :simple :foreign-array element-type))
           (fa-typed-vclass (vector-class :simple :foreign-array element-type))
           (fa-typed-base-mclass (matrix-class :base :foreign-array element-type))
           (fa-typed-base-vclass (vector-class :base :foreign-array element-type))
           (fnv-mclass (make-symbol* "FNV-" fnv-type))
           (fnv-ref (make-symbol* "FNV-" fnv-type "-REF"))
           (make-fnv (make-symbol* "MAKE-FNV-" fnv-type)))
      `(progn
         
         (make-matrix-class-hierarchy :foreign-array ,element-type)
         (make-vector-class-hierarchy :foreign-array ,element-type)
         
         (defclass ,fa-typed-mclass (,fa-typed-base-mclass)
           ((data :initarg :data
                  :accessor data
                  :type ,fnv-mclass
                  :documentation "The FNV object holding the
                  elements."))
           (:documentation ,(format nil "Dense matrix holding ~
            elements of type ~A, implemented as a foreign array."
                                    element-type)))

         (defclass ,fa-typed-vclass (,fa-typed-base-vclass
                                     ,fa-typed-mclass)
           ()
           (:documentation ,(format nil "Dense vector holding ~
            elements of type ~A, implemented as a foreign array."
                                    element-type)))
         
         (defmethod mref ((matrix ,fa-typed-mclass) i j)
           (,fnv-ref (data matrix)
                     (flatten-matrix-indices matrix i j)))

         (defmethod (setf mref) (value (matrix ,fa-typed-mclass) i j)
           (setf (,fnv-ref (data matrix)
                           (flatten-matrix-indices matrix i j))
                 value))

         (defmethod vref ((vector ,fa-typed-mclass) i)
           (,fnv-ref (data vector) i))

         (defmethod (setf vref) (value (vector ,fa-typed-mclass) i)
           (setf (,fnv-ref (data vector) i) value))
         
         (defmethod make-fa-matrix (nrows ncols
                                    (fnv-type (eql ',fnv-type))
                                    &key initial-element)
           (let ((data (,make-fnv (* nrows ncols)
                                  :initial-value initial-element)))
             (if (or (= nrows 1) (= ncols 1))
                 (make-instance ',fa-typed-vclass :nrows nrows
                                :ncols ncols :data data)
                 (make-instance ',fa-typed-mclass :nrows nrows
                                :ncols ncols :data data))))))))

(construct-fa-matrix single-float)
(construct-fa-matrix double-float)
(construct-fa-matrix (complex single-float))
(construct-fa-matrix (complex double-float))

;;; These don't exist yet...!
;;(construct-fa-matrix fixnum)
;;(construct-fa-matrix integer)
