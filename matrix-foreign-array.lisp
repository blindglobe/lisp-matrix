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
    (car (rassoc element-type *fnv-type-table* :test #'equal)))

  (defun fa-matrix-class (element-type &optional (type :simple))
    "Return the FA-MATRIX class name corresponding to ELEMENT-TYPE."
    (make-symbol* (ecase type
                    (:base "FA-MATRIX-")
                    (:simple "FA-SIMPLE-MATRIX-")
                    (:matview "FA-MATVIEW-")
                    (:transpose "FA-TRANSPOSE-MATVIEW-")
                    (:window "FA-WINDOW-MATVIEW-")
                    (:strided "FA-STRIDED-MATVIEW-"))
                  (element-type->fnv-type element-type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro construct-fa-matrix (fnv-type)
    (let* ((element-type (fnv-type->element-type fnv-type))
           (fa-class (fa-matrix-class element-type))
           (fa-base-class (fa-matrix-class element-type :base))
           (fa-transpose-class (fa-matrix-class element-type
                                                :transpose))           
           (fa-window-class (fa-matrix-class element-type :window))
           (fa-strided-class (fa-matrix-class element-type :strided))
           (fnv-class (make-symbol* "FNV-" fnv-type))
           (fnv-ref (make-symbol* "FNV-" fnv-type "-REF"))
           (make-fnv (make-symbol* "MAKE-FNV-" fnv-type)))
      `(progn
         (defclass ,fa-base-class (fa-matrix)
           ()
           (:documentation ,(format nil "Base class for dense ~
           matrices holding elements of type ~A, implemented as a ~
           foreign array." element-type)))
         
         (defclass ,fa-class (,fa-base-class)
           ((data :initarg :data
                  :accessor data
                  :type ,fnv-class
                  :documentation "The FNV object holding the
                  elements."))
           (:documentation ,(format nil "Dense matrix holding ~
            elements of type ~A, implemented as a foreign array."
            element-type)))
         
         (defmethod mref ((matrix ,fa-class) i j)
           (,fnv-ref (data matrix)
                     (flatten-matrix-indices matrix i j)))

         (defmethod (setf mref) (value (matrix ,fa-class) i j)
           (setf (,fnv-ref (data matrix)
                           (flatten-matrix-indices matrix i j))
                 value))
         
         (defclass ,fa-transpose-class (,fa-base-class
                                        transpose-matview)
           ()
           (:documentation ,(format nil "Transposed view of a ~A ~
           matrix." fa-class)))

         (defclass ,fa-window-class (,fa-base-class window-matview)
           ()
           (:documentation ,(format nil "Windowed view of a ~A ~
           matrix." fa-class)))

         (defclass ,fa-strided-class (,fa-base-class strided-matview)
           ()
           (:documentation ,(format nil "Strided view of a ~A ~
           matrix." fa-class)))

         (defmethod transpose-class ((matrix ,fa-base-class))
           ',fa-transpose-class)
         
         (defmethod window-class ((matrix ,fa-base-class))
           ',fa-window-class)
         
         (defmethod stride-class ((matrix ,fa-base-class))
           ',fa-strided-class)

         (defmethod element-type ((matrix ,fa-base-class))
           ',element-type)
         
         (defmethod make-fa-matrix (nrows ncols
                                    (fnv-type (eql ',fnv-type))
                                    &key initial-element)
           (let ((data (,make-fnv (* nrows ncols)
                                  :initial-value initial-element)))
             (make-instance ',fa-class :nrows nrows :ncols ncols
                            :data data)))))))

(construct-fa-matrix double)
(construct-fa-matrix float)
(construct-fa-matrix complex-double)
(construct-fa-matrix complex-float)
