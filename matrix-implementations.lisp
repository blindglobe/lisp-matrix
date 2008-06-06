(in-package :lisp-matrix)

;;;; (following from matrix.lisp)
;;;;
;;;; * Define implementations
;;;;
;;;; In order to avoid code duplication in implementation, we
;;;; introduce a macro that will build classes for the matrix views.
;;;;
;;;; We will assume that the class dependencies are like this for each
;;;; implementation element type, where we add a prefix "IMPL" for the
;;;; implementation and a postfix "ET" for the element-type.
;;;;
;;;; - matrix-et-like
;;;;   -  impl-matrix-et
;;;;      - impl-simple-matrix-et
;;;;      - impl-matview-et
;;;;        - impl-transpose-matview-et
;;;;        - impl-window-matview-et
;;;;        - impl-strided-matview-et
;;;;
;;;; The implementation must implement the class
;;;; IMPL-SIMPLE-MATRIX-ET, and the following macro takes care of the
;;;; other classes, and implements also the method ELEMENT-TYPE.

(defun matrix-class (class-type &optional implementation-shortname
                     element-type-shortname)
  "Return the matrix class name corresponding to CLASS-TYPE.  When
  IMPLEMENTATION-SHORTNAME is given, it gives the class for the given
  implementation, and when ELEMENT-TYPE-SHORTNAME is given, a
  specialized class for that element type."
  (let ((type-string
         (ecase class-type
           (:base "MATRIX")
           (:simple "SIMPLE-MATRIX")
           (:matview "MATVIEW")
           (:transpose "TRANSPOSE-MATVIEW")
           (:window "WINDOW-MATVIEW")
           (:strided "STRIDED-MATVIEW"))))
   (cond ((and implementation-shortname element-type-shortname)
          (make-symbol* implementation-shortname "-" type-string "-"
                        element-type-shortname))
         (implementation-shortname
          (make-symbol* implementation-shortname "-" type-string))
         (element-type-shortname
          (make-symbol* type-string "-" element-type-shortname))
         (t
          (make-symbol* type-string)))))

(defmacro make-class-hierarchy (implementation
                                implementation-shortname
                                element-type
                                element-type-shortname)
  (let* ((is implementation-shortname)
         (es element-type-shortname)
         (impl-base-class (matrix-class :base is nil))
         (typed-base-class (matrix-class :base nil es))
         (typed-impl-base-class (matrix-class :base is es))
         (typed-class (matrix-class :simple is es))
         (typed-transpose-class (matrix-class :transpose is es))
         (typed-window-class (matrix-class :window is es))
         (typed-strided-class (matrix-class :strided is es)))
    `(progn

       (defclass ,typed-base-class (matrix-like)
         ()
         (:documentation ,(format nil "Base class for matrices ~
         holding elements of type ~A." element-type)))
       
       (defclass ,typed-impl-base-class (,impl-base-class ,typed-base-class)
         ()
         (:documentation ,(format nil "Base class for dense ~
           matrices holding elements of type ~A for the ~
           implementation ~A." element-type implementation)))
         
       (defclass ,typed-transpose-class (,typed-impl-base-class
                                         transpose-matview)
         ()
         (:documentation ,(format nil "Transposed view of a ~A ~
           matrix." typed-class)))

       (defclass ,typed-window-class (,typed-impl-base-class window-matview)
         ()
         (:documentation ,(format nil "Windowed view of a ~A ~
           matrix." typed-class)))

       (defclass ,typed-strided-class (,typed-impl-base-class strided-matview)
         ()
         (:documentation ,(format nil "Strided view of a ~A ~
           matrix." typed-class)))

       (defmethod transpose-class ((matrix ,typed-impl-base-class))
         ',typed-transpose-class)
         
       (defmethod window-class ((matrix ,typed-impl-base-class))
         ',typed-window-class)
         
       (defmethod stride-class ((matrix ,typed-impl-base-class))
         ',typed-strided-class)

       (defmethod element-type ((matrix ,typed-impl-base-class))
         ',element-type))))
