(in-package :lisp-matrix)

;;;; This file contains functions and macros to help build LAPACK
;;;; wrapper methods.
;;;;
;;;; Time-stamp: <2008-06-06 11:39:55 Evan Monroig>
;;;;
;;;;
;;;;
;;;;
;;;; * when to copy for GEMM ?
;;;; 
;;;; - base matrix => no
;;;; 
;;;; - transposed base matrix (even multiple times) => no
;;;; 
;;;; - windowed matrix: will depend on the offsets, if both offsets
;;;; are zero then we don't need to copy, as the parameters LDA, LDB
;;;; and LDC can be used to specify the actual number of rows.  As for
;;;; the actual number of columns, it is ok if there are more actual
;;;; columns than the number we tell to LAPACK.
;;;;
;;;; TODO: If both offsets are not zero, then we need to pass a
;;;; pointer to the start of the data instead of to the array itself,
;;;; and take into account the associated column stride for the
;;;; parameters LDA, LDB and LDC.
;;;;
;;;; - in the same way, combinations of windows and transposes should
;;;; work as long as the windows have zero offset.
;;;;
;;;; - for strides, it is the same as long as offsets are 0 and
;;;; strides are 1, i.e. when the stride actually is a window.
;;;;
;;;; Note: for the matrix C, we cannot define the orientation of the
;;;; matrix, so transposed matrices would have to be copied anyway if
;;;; the orientation is not :COLUMN.
;;;;
;;;;
;;;; I am not sure if there are other restrictions for when we can
;;;; copy matrices for use with LAPACK functions, except the need to
;;;; copy when the arguments may be modified and we don't want to
;;;; modify the matrices.  We may wish to provide destructive and
;;;; non-destructive versions of each LAPACK operators.
;;;;
;;;; For dgemm, zgemm, sgemm and cgemm there would then be two generic
;;;; functions, namely GEMM and GEMM!.  The two could be defined
;;;; simultaneously using the same macro call.
;;;;
;;;; In the DSL to generate lapack methods we introduce a macro
;;;; WITH-COPIES, which used the functions COPY, COPY! and COPY-MAYBE
;;;; introduced in matrix.lisp.  We can use more general predicates
;;;; using OR, AND and NOT, which would be the more common uses.
;;;;
;;;; Finally, for example for GEMM!, after the copies are made we need
;;;; to inspect the resulting matrices to tell LAPACK the actual sizes
;;;; and orientations of the matrices.
;;;;
;;;; * CFFI and fortran types
;;;;
;;;; The package cl-blapack uses CFFI to access the fortran libraries
;;;; BLAS and LAPACK.  As it is currently implemented, several CFFI
;;;; types are defined.
;;;;
;;;; For example, FORTRAN-INT and FORTRAN-DOUBLE are scalar types,
;;;; which may be passed as unboxed integers or double-float to
;;;; fortran through FFI.  However, if fortran modifies their values
;;;; and we want to know the results, we have to pass them as arrays
;;;; with one element and read the array to read back their value.
;;;;
;;;; I did this for example in the method GELSY (a least-squares
;;;; solver in lapack using a variant of the QR method) in the file
;;;; `lapack-methods.lisp', using MAKE-FNV-INT32 to make an array
;;;; containing one integer and FNV-INT32-REF to retrieve its value.
;;;;
;;;; Then there are vector types such as CFFI-FNV-DOUBLE, which are
;;;; arrays allocated by CFFI through the foreign-numeric-vector (FNV)
;;;; package.  This is fine for matrices of implementation
;;;; :FOREIGN-ARRAY, since they are based on FNV vectors anyway.
;;;;
;;;; But what to do with matrices based on lisp arrays?  Actually the
;;;; type CFFI-FNV-DOUBLE is just an alias to the CFFI type :POINTER.
;;;; When we define new CFFI types, we must also define how to convert
;;;; between lisp types and CFFI types.  There are two ways to do this.
;;;;
;;;; 1. The first method is to use the CFFI generic functions
;;;;    TRANSLATE-TO-FOREIGN and TRANSLATE-FROM-FOREIGN.  A call to
;;;;    the CFFI macro DEFCFUN actually expands into a DEFUN in which
;;;;    each argument is converted by TRANSLATE-TO-FOREIGN before
;;;;    calling the foreign function, and then converted back via
;;;;    TRANSLATE-FROM-FOREIGN after calling the foreign function, or
;;;;    freeed using FREE-TRANSLATED-OBJECT (another generic
;;;;    function).  In addition to that, in cl-blapack and
;;;;    foreign-numeric-vector, for the fortran types mentioned above
;;;;    the generic function TRANSLATE-TO-FOREIGN is implemented as a
;;;;    call to the generic function FNV-FOREIGN-POINTER.  So for each
;;;;    call to a foreign fortran function, we call at least three
;;;;    generic functions for each argument.  That may or may not be a
;;;;    lot, depending on the size of the matrices and the level of
;;;;    the operations involved (BLAS level 1/2/3 or LAPACK).  For
;;;;    small matrices, this is a huge overhead; for large matrices,
;;;;    it is next to nothing.
;;;;
;;;;    What this gives to us is that if we wish to directly give the
;;;;    lapack function pointers to a lisp array, we almost just have
;;;;    to specialize on the function TRANSLATE-TO-FOREIGN to return a
;;;;    pointer to the data of the matrix.  Almost, because we also
;;;;    need to tell the lisp to (please) not move around our data
;;;;    because if then fortran accesses it it's the greatest route to
;;;;    catastrophe.  So we need to wrap around a macro with a name
;;;;    like WITH-PINNED-ARRAY for each lisp array that we want the
;;;;    foreign function to access.
;;;;
;;;; 2. The second method is to use the CFFI generic functions
;;;;    CONVERT-TO-FOREIGN and CONVERT-FROM-FOREIGN.  They are there
;;;;    to inline the type conversions into the DEFUN that is
;;;;    generated by DEFCFUN.  So we don't need to call 3 generic
;;;;    functions for each argument at each call after all: just
;;;;    FNV-FOREIGN-POINTER, but that can be inlined too.
;;;;
;;;;    The problem is that since we inline the type conversions, we
;;;;    cannot anymore have the same code for matrices based on lisp
;;;;    arrays and matrices based on foreign arrays.  But for lisp
;;;;    arrays we have to wrap a macro WITH-PINNED-ARRAY anyways, so
;;;;    the code is splitted anyway, although not all the way down to
;;;;    the calls to the macro DEFCFUN...
;;;;
;;;; So my proposition is to go for 1. at first since it requires just
;;;; to add some TRANSLATE-TO-FOREIGN methods, and later do 2. by
;;;; doing the conversions ourselves instead of going through the
;;;; macro DEFCFUN.

(defun make-predicate (form)
  "From an expression combining predicates, construct a function of
  one argument that evaluates the logical expression on the element,
  where each predicate is applied to the argument to obtain its
  logical value.

  FORM may be constructed as follows: a symbol whose f-value is a
  function of one argument; a list whose car is 'OR and whose CDR is a
  list of predicates; a list whose car is 'AND and whose CDR is a list
  of predicates; T; NIL."
  (typecase form
    (symbol
     (case form
       ((t) '(constantly t))
       ((nil) '(constantly nil))
       (t form)))
    (list
     (labels ((aux (arg)
                (etypecase arg
                  (symbol (list arg 'a))
                  (list
                   (ecase (car arg)
                     (or (cons 'or (mapcar #'aux (cdr arg))))
                     (and (cons 'and (mapcar #'aux (cdr arg))))
                     (not (list 'not (aux (cadr arg)))))))))
       `(lambda (a)
          ,(aux form))))))

(defmacro with-copies ((&rest forms) result &body body)
  "Each form in FORMS is a lambda-list defined as (VARIABLE PREDICATE
  &optional COPY-BACK-P).  VARIABLE is a symbol bound to a matrix,
  that is to be copied if the predicate obtained from PREDICATE
  applied to the matrix is true.  All variables are bound
  to (possible) copies of the original matrices, and body is executed.
  After that, variables for which COPY-BACK-P is true are copied back
  to the original matrices, and the evaluation of RESULT is returned
  with the variables bound to the original matrices.

  The PREDICATE value may be constructed as follows: a symbol whose
  f-value is a function of one argument; a list whose car is 'OR and
  whose CDR is a list of predicates; a list whose car is 'AND and
  whose CDR is a list of predicates; T; NIL.

  See the file `lapack-methods.lisp' for examples of use."
  (let ((gensyms (loop for form in forms collect
                       (gensym (symbol-name (first form))))))
    `(progn
       (let (,@(mapcar (lambda (form gensym)
                         (list gensym (car form)))
                       forms gensyms))
         (let (,@(mapcar
                  (lambda (form gensym)
                    (destructuring-bind
                          (variable predicate &optional copy-back-p)
                        form
                      (declare (ignore copy-back-p))
                      `(,variable
                        (copy-maybe ,gensym
                                    ,(make-predicate predicate)))))
                  forms gensyms))
           ,@body
           ,@(loop for form in forms
                   for g in gensyms
                   when (third form)
                   collect `(copy! ,(first form) ,g))))
       ,result)))

(defparameter *supported-datatypes*
  '((float . "S")
    (double . "D")
    (complex-float . "C")
    (complex-double . "Z"))
  "Association list mapping each supported datatype to its BLAS/LAPACK
  letter.")

(defun datatype->letter (datatype)
  "Converts the given DATATYPE to the letter that symbolizes it in the
  BLAS and LAPACK."
  (or (cdr (assoc datatype *supported-datatypes* :test #'equal))
      (error "LAPACK does not support the datatype ~A" datatype)))

(defmacro def-lapack-method (name (&rest lambda-list) &body body)
  "Define methods for supported datatypes for the lapack method named
  NAME.  The symbols !FUNCTION, !DATA-TYPE, and !MATRIX-TYPE are
  respectively bound to the actual lapack function to be called from
  the package CL-BLAPACK, the data type (float, double, complex-float
  or complex-double), and the corresponding abstract matrix
  type (e.g. matrix-double-like).

  See for example the definition of GEMM for how to use this macro."
  ;; FIXME: I don't like the fact that this macro uses templates, but
  ;; the code works. -- Evan Monroig 2008-05-04
  ;; FIXME: also create the generic function with documentation --
  ;; Evan Monroig 2008-05-04
  `(progn
     ,@(loop for (type . type-letter) in *supported-datatypes*
             collect
             (let* ((element-type (fnv-type->element-type type))
                    (replacements
                     `((!function . ,(make-symbol* "%" type-letter name))
                       (!data-type . ,type)
                       (!element-type . ,element-type)
                       (!matrix-type . ,(matrix-class :base :foreign-array
                                                      element-type)))))
               `(defmethod ,name
                    ,(sublis replacements lambda-list)
                  (with-blapack
                    ,@(sublis replacements body)))))))

(defun orientation->letter (orientation)
  "Return the LAPACK letter corresponding to ORIENTATION."
  (ecase orientation
    (:column "N")
    (:row "T")))

(defun orientation-letter (matrix)
  "Return the LAPACK letter corresponding to the orientation of
  MATRIX."
  (orientation->letter (orientation matrix)))
