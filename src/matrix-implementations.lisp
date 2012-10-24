(in-package :lisp-matrix)

;;;; (following from matrix.lisp)
;;;;
;;;; * Define implementations
;;;;
;;;; ** Implementation
;;;;
;;;; We will introduce a simple function DEFINE-IMPLEMENTATION to
;;;; maintain a list of available implementations, and also keep some
;;;; information on them.  For now, we will only store the short name
;;;; of the implementation that is used in class prefixes.

(defvar *implementations* nil
  "Table of available implementations.")

(defun implementation-short-name (implementation)
  (cdr (assoc implementation *implementations*)))

(defun define-implementation (keyword short-name)
  "Define an implementation named KEYWORD and with SHORT-NAME (a
  string) as abbreviated name."
  (pushnew (cons keyword short-name) *implementations*
           :test #'equal))

;;;; ** Element types
;;;;
;;;; Although some lisp types are symbols, such as 'DOUBLE-FLOAT, some
;;;; others are lists, such as '(COMPLEX DOUBLE-FLOAT).  In our matrix
;;;; classes, we sometimes want to have generic functions specializing
;;;; on an element-type argument using EQL, so it should be a symbol.
;;;; Also, we would like to unify the matrix class names by having
;;;; typed matrices names containing a description of the element
;;;; type.
;;;;
;;;; To solve these two problems, we will define a correspondence
;;;; between lisp types and symbols representing these lisp types.  We
;;;; will call such symbols lisp-matrix types by opposition to lisp
;;;; types.
;;;
;;;; For each symbol thus created, we create a type of the
;;;; corresponding name if it does not already exist, and we provide
;;;; functions to associate a lisp type to a lisp-matrix type.
;;;;
;;;; We add some additional information to each type,

(defvar *type-table* nil
  "Table with information about lisp-matrix types, which are used in
  the class names of typed matrices and correspond to their element
  type.")

(defstruct type-info
  (lisp-type t :type (or symbol cons))
  (lisp-matrix-type t :type symbol)
  (cffi-type nil :type symbol)
  (size 0 :type (unsigned-byte 32)))

(defun add-type (&key (lisp-type t) (lisp-matrix-type t) cffi-type)
  (pushnew (make-type-info :lisp-matrix-type lisp-matrix-type
                           :lisp-type lisp-type
                           :cffi-type cffi-type
                           :size (if cffi-type
                                     (cffi:foreign-type-size cffi-type)
                                     0))
           *type-table*
           :key #'type-info-lisp-matrix-type))

(defun lisp-type-info (lisp-type)
  (find lisp-type *type-table*
        :key #'type-info-lisp-type
        :test #'equal))

(defun lisp-matrix-type-info (lisp-matrix-type)
  (find lisp-matrix-type *type-table*
        :key #'type-info-lisp-matrix-type))

(defun lisp-type-size (lisp-type)
  (type-info-size (lisp-type-info lisp-type)))

(defun lisp-matrix-type-size (lisp-matrix-type)
  (type-info-size (lisp-matrix-type-info lisp-matrix-type)))

(defun lisp-type->lisp-matrix-type (lisp-type)
  "Return the LISP-MATRIX-TYPE corresponding to the lisp type
  LISP-TYPE."
  (let ((info (lisp-type-info lisp-type)))
    (when (type-info-p info)
      (type-info-lisp-matrix-type info))))

(defun lisp-matrix-type->lisp-type (lisp-matrix-type)
  "Return the lisp type corresponding to LISP-MATRIX-TYPE."
  (let ((info (lisp-matrix-type-info lisp-matrix-type)))
    (when (type-info-p info)
      (type-info-lisp-type info))))

(defmacro def-lisp-matrix-type (name lisp-type &key cffi-type)
  "Define a new lisp-matrix type of name NAME from the lisp type
  TYPE."
  (check-type name symbol)
  (check-type cffi-type symbol)
  (check-type lisp-type (or symbol cons))
  `(progn
     (add-type :lisp-type ',lisp-type :lisp-matrix-type ',name
               :cffi-type ',cffi-type)
           
     ,(unless (eql name lisp-type)
              `(deftype ,name () ',lisp-type))))

(def-lisp-matrix-type single single-float :cffi-type :float)
(def-lisp-matrix-type double double-float :cffi-type :double)
(def-lisp-matrix-type complex-single (complex single-float) :cffi-type fnv:cffi-fnv-complex-float)
(def-lisp-matrix-type complex-double (complex double-float) :cffi-type fnv:cffi-fnv-complex-double)
(def-lisp-matrix-type fixnum fixnum)
(def-lisp-matrix-type integer integer)
(def-lisp-matrix-type t t)

;;;; ** Matrix classes
;;;;
;;;; In order to avoid code duplication in implementation, we
;;;; introduce a macro that will build classes for the matrix views.
;;;;
;;;; We will assume that the class dependencies are like this for each
;;;; implementation element type, where we add a prefix "IMPL" (eg, we
;;;; use fa, la for respectively, foriegn-stored and lisp-stored
;;;; arrays) for the implementation and element-type described by a
;;;; postfix "ET" (eg for real, complex, double, etc).
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

(defun matrix-class (class-type &optional implementation element-type)
  "Return the matrix class name corresponding to CLASS-TYPE.  When
  IMPLEMENTATION-SHORTNAME is given, it gives the class for the given
  implementation, and when ELEMENT-TYPE-SHORTNAME is given, a
  specialized class for that element type."
  (let ((is (when implementation
              (implementation-short-name implementation)))
        (es (when element-type
              (symbol-name (lisp-type->lisp-matrix-type element-type)))))
    (let ((type-string
           (ecase class-type
             (:base "MATRIX")
             (:simple "SIMPLE-MATRIX")
             (:matview "MATVIEW")
             (:transpose "TRANSPOSE-MATVIEW")
             (:window "WINDOW-MATVIEW")
             (:strided "STRIDED-MATVIEW"))))
      (cond ((and is es)
             (make-symbol* is "-" type-string "-" es))
            (is
             (make-symbol* is "-" type-string))
            (es
             (make-symbol* type-string "-" es))
            (t
             (make-symbol* type-string))))))

(defun vector-class (class-type &optional implementation element-type)
  "Return the vector class name corresponding to CLASS-TYPE.  When
  IMPLEMENTATION-SHORTNAME is given, it gives the class for the given
  implementation, and when ELEMENT-TYPE-SHORTNAME is given, a
  specialized class for that element type."
  (let ((is (when implementation
              (implementation-short-name implementation)))
        (es (when element-type
              (symbol-name (lisp-type->lisp-matrix-type element-type)))))
    (let ((type-string
           (ecase class-type
             (:base "VECTOR")
             (:simple "SIMPLE-VECTOR")
             (:vecview "VECVIEW")
             (:transpose "TRANSPOSE-VECVIEW")
             (:slice "SLICE-VECVIEW"))))
      (cond ((and is es)
             (make-symbol* is "-" type-string "-" es))
            (is
             (make-symbol* is "-" type-string))
            (es
             (make-symbol* type-string "-" es))
            (t
             (make-symbol* type-string))))))

(defmacro make-matrix-class-hierarchy (implementation element-type)
  (let* ((impl-base-class (matrix-class :base implementation nil))
         (typed-base-class (matrix-class :base nil element-type))
         (typed-impl-base-class
          (matrix-class :base implementation element-type))
         (typed-class
          (matrix-class :simple implementation element-type))
         (typed-transpose-class
          (matrix-class :transpose implementation element-type))
         (typed-window-class
          (matrix-class :window implementation element-type))
         (typed-strided-class
          (matrix-class :strided implementation element-type)))
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
         
       (defclass ,typed-transpose-class (transpose-matview
                                         ,typed-impl-base-class)
         ()
         (:documentation ,(format nil "Transposed view of a ~A ~
           matrix." typed-class)))

       (defclass ,typed-window-class (window-matview ,typed-impl-base-class)
         ()
         (:documentation ,(format nil "Windowed view of a ~A ~
           matrix." typed-class)))

       (defclass ,typed-strided-class (strided-matview ,typed-impl-base-class)
         ()
         (:documentation ,(format nil "Strided view of a ~A ~
           matrix." typed-class)))

       (defmethod transpose-class ((matrix ,typed-impl-base-class))
         ',typed-transpose-class)
         
       (defmethod window-class ((matrix ,typed-impl-base-class))
         ',typed-window-class)
         
       (defmethod stride-class ((matrix ,typed-impl-base-class))
         ',typed-strided-class)

       (defmethod element-type ((matrix ,typed-base-class))
         ',element-type)

       (defmethod element-type-size ((matrix ,typed-base-class))
         ,(lisp-type-size element-type)))))

(defmacro make-vector-class-hierarchy (implementation element-type)
  (let* ((impl-base-vclass (vector-class :base implementation nil))
         (typed-base-mclass (matrix-class :base nil element-type))
         (typed-base-vclass (vector-class :base nil element-type))
         (typed-impl-base-vclass
          (vector-class :base implementation element-type))
         (typed-impl-base-mclass
          (matrix-class :base implementation element-type))
         (typed-vclass
          (vector-class :simple implementation element-type))
         (typed-transpose-vclass
          (vector-class :transpose implementation element-type))
         (typed-slice-vclass
          (vector-class :slice implementation element-type)))
    `(progn

       (defclass ,typed-base-vclass (vector-like ,typed-base-mclass)
         ()
         (:documentation ,(format nil "Base class for vectors ~
         holding elements of type ~A." element-type)))
       
       (defclass ,typed-impl-base-vclass (,impl-base-vclass
                                          ,typed-base-vclass
                                          ,typed-impl-base-mclass)
         ()
         (:documentation ,(format nil "Base class for dense ~
           vectors holding elements of type ~A for the ~
           implementation ~A." element-type implementation)))
         
       (defclass ,typed-transpose-vclass (transpose-vecview
                                         ,typed-impl-base-vclass)
         ()
         (:documentation ,(format nil "Transposed view of a ~A ~
           vector." typed-vclass)))

       (defclass ,typed-slice-vclass (slice-vecview
                                      ,typed-impl-base-vclass)
         ()
         (:documentation ,(format nil "Slice view of a ~A ~
           vector." typed-vclass)))

       (defmethod transpose-class ((matrix ,typed-impl-base-vclass))
         ',typed-transpose-vclass)
         
       (defmethod slice-class ((matrix ,typed-impl-base-mclass))
         ',typed-slice-vclass))))
