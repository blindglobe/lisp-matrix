(in-package :lisp-matrix)

;;; This file contains functions and macros to help build LAPACK
;;; wrapper methods.
;;;
;;; Time-stamp: <2008-05-11 09:13:22 Evan Monroig>
;;;
;;;
;;;
;;;
;;; when to copy for GEMM ?
;;; 
;;; - base matrix => no
;;; 
;;; - transposed base matrix (even multiple times) => no
;;; 
;;; - windowed matrix: will depend on the offsets, if both offsets are
;;; zero then we don't need to copy, as the parameters LDA, LDB and LDC
;;; can be used to specify the actual number of rows.  As for the
;;; actual number of columns, I think that it should be ok if there are
;;; more actual columns than the number we tell to LAPACK.
;;;
;;; - in the same way, combinations of windows and transposes should
;;; work as long as the windows have zero offset.
;;;
;;; - for strides, it is the same as long as offsets are 0 and strides
;;; are 1, i.e. when the stride actually is a window.
;;;
;;; Note: for the matrix C, we cannot define the orientation of the
;;; matrix, so transposed matrices would have to be copied.
;;;
;;;
;;; I am not sure if there are other restrictions for when we can copy
;;; matrices for use with LAPACK functions, except the need to copy
;;; when the arguments may be modified and we don't want to modify the
;;; matrices.  We may wish to provide destructive and non-destructive
;;; versions of each LAPACK operators.
;;;
;;; For dgemm, zgemm, sgemm and cgemm there would then be two generic
;;; functions, namely GEMM and GEMM!.  The two could be defined
;;; simultaneously using the same macro call.
;;;
;;; For the copy functions, we can define one COPY function which will
;;; return a copy, a function COPY-INTO to copy a matrix into another
;;; when for example GEMM! is called with a matrix C that has to be
;;; called but is supposed to be destructively modified.  COPY can be
;;; implemented in terms of COPY-INTO.
;;;
;;; Then, we would implement functions COPY-MAYBE and COPY-INTO-MAYBE
;;; which take a predicate as argument to copy the matrix only if
;;; needed.  If in the DSL to generate lapack methods we introduce a
;;; macro WITH-COPIES, we can use more general predicates using OR, AND
;;; and NOT, which would be the more common uses.
;;;
;;; Finally, for example for GEMM!, after the copies are made we need
;;; to inspect the resulting matrices to tell LAPACK the actual sizes
;;; and orientations of the matrices.


(defun make-predicate (form)
  "From an expression combining predicates, construct a function of
one argument that evaluates the logical expression on the element,
where each predicate is applied to the argument to obtain its logical
value.

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
&optional COPY-BACK-P).  VARIABLE is a symbol bound to a matrix, that
is to be copied if the predicate obtained from PREDICATE applied to
the matrix is true.  All variables are bound to (possible) copies of
the original matrices, and body is executed.  After that, variables
for which COPY-BACK-P is true are copied back to the original
matrices, and the evaluation of RESULT is returned with the variables
bound to the original matrices.

The PREDICATE value may be constructed as follows: a symbol whose
f-value is a function of one argument; a list whose car is 'OR and
whose CDR is a list of predicates; a list whose car is 'AND and whose
CDR is a list of predicates; T; NIL."
  (let ((gensyms (loop for form in forms collect
                      (gensym (symbol-name (first form))))))
    `(progn
       (let (,@(mapcar (lambda (form gensym)
                         (list gensym (car form)))
                       forms gensyms))
         (let (,@(mapcar (lambda (form gensym)
                           (destructuring-bind (variable predicate &optional copy-back-p)
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
                collect `(copy-into ,(first form) ,g))))
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
respectively bound to the actual lapack function to be called from the
package CL-BLAPACK, the data type (float, double, complex-float or
complex-double), and the corresponding abstract matrix
type (e.g. matrix-double-like).

See for example the definition of GEMM for how to use this macro."
  ;; FIXME: I don't like the fact that this macro uses templates, but
  ;; the code works. -- Evan Monroig 2008-05-04
  ;; FIXME: also create the generic function with documentation --
  ;; Evan Monroig 2008-05-04
  `(progn
     ,@(loop for (type . type-letter) in *supported-datatypes*
          collect
          (let ((replacements
                 `((!function . ,(make-symbol* "%" type-letter name))
                   (!data-type . ,type)
                   (!matrix-type . ,(fnv-type-to-matrix-type type :base)))))
            `(defmethod ,name
                 ,(sublis replacements lambda-list)
               (with-blapack
                 ,@(sublis replacements body)))))))

(defun orientation->letter (orientation)
  "Return the LAPACK letter corresponding to ORIENTATION."
  (ecase orientation
    (:column "N")
    (:row "T")))

(defun orientation-letter (a)
  "Return the LAPACK letter corresponding to the orientation of the
matrix A."
  (orientation->letter (orientation a)))
