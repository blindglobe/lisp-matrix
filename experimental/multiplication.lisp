;;;
;;; Time-stamp: <2008-06-07 15:54:15 Evan Monroig>

;;;; This is some experimental code to call the fortran dgemm using
;;;; CFFI types for which lisp arrays are converted inline (no generic
;;;; functions) -- this would necessitate to duplicate the work (and
;;;; the CFFI types) for the case of foreign arrays.


(in-package :lisp-matrix)

(asdf:oos 'asdf:load-op 'ffa)
(asdf:oos 'asdf:load-op 'cl-utilities)

(import '(blapack-cffi-types:fortran-int
          blapack-cffi-types:fortran-double))

(define-foreign-type lisp-array-double-type
    ()
  ()
  (:ACTUAL-TYPE :POINTER)
  (:SIMPLE-PARSER lisp-array-double))

(defmethod expand-to-foreign-dyn (value var body
                                  (type lisp-array-double-type))
  (cl-utilities:once-only (value)
   `(ffa:with-pointer-to-array ((data ,value) ,var :double (nelts
                                  ,value) :copy-in)
      ,@body)))

(defmethod gemm (alpha (a la-matrix-double) (b la-matrix-double) beta
                 (c la-matrix-double))
  (assert (= (ncols a) (nrows b)))
  (assert (= (nrows a) (nrows c)))
  (assert (= (ncols b) (ncols c)))
  (with-copies ((a (or (not unit-stride-p)
                       (not zero-offset-p)))
                (b (or (not unit-stride-p)
                       (not zero-offset-p)))
                (c (or (not unit-stride-p)
                       (not zero-offset-p)
                       transposed-p)
                   t))
      c
    (foreign-funcall "dgemm_"
                     :string (orientation-letter a)
                     :string (orientation-letter b)
                     fortran-int (nrows a)
                     fortran-int (ncols b)
                     fortran-int (ncols a)
                     fortran-double alpha
                     lisp-array-double a
                     fortran-int (real-nrows a)
                     lisp-array-double b
                     fortran-int (real-nrows b)
                     fortran-double beta
                     lisp-array-double c
                     fortran-int (real-nrows c))))
