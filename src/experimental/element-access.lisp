;;; Time-stamp: <2008-05-20 18:12:38 Evan Monroig>
;;; 
;;; Here I made several experiments to see how to improve vector and
;;; matrix element access.
;;;
;;;

(in-package :lisp-matrix)

(import 'cl-utilities:once-only)
(import 'cl-utilities:with-unique-names)

(defconstant +asize+ 4000000)
(declaim (type fixnum +asize+))


(defun lisp-matrix-vref-benchmark ()
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (dotimes (i +ASIZE+ s)
      (declare (type fixnum i))
      (incf s (vref a i)))))

#+nil
(time (lisp-matrix-vref-benchmark))
;; => 0.727 s

;;; 1) Return a lambda function.
;;;
;;; This was to see the difference in cost between the generic VREF
;;; and a VREF using a normal function call

(defmethod vref-lambda ((a vector-double))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (lambda (a i)
    (declare (type fixnum i)
             (type vector-double a))
    (the double-float (fnv-double-ref (data a) i))))

(defun lisp-matrix-vref-lambda-benchmark ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (let ((vref (vref-lambda a)))
      (declare (type function vref))
      (dotimes (i +asize+ s)
        (declare (type fixnum i))
        (incf s (the double-float (funcall vref a i)))))))

#+nil
(time (lisp-matrix-vref-lambda-benchmark))
;; => 0.353 s

;;; 2) Use a mapping operator.
;;;
;;; Basically the cost is the same as above.  Instead of having a call
;;; to VREF each time, we have instead a call to the function to
;;; execute on the current element.

(defmethod mapc-matrix (function (a vector-double))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (i (nelts a))
    (declare (type fixnum i))
    (funcall function (fnv-double-ref (data a) i))))

(defun lisp-matrix-mapc-matrix-benchmark ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (mapc-matrix (lambda (elt)
                   (declare (type double-float elt))
                   (incf s elt))
                 a)
    s))

#+nil
(time (lisp-matrix-mapc-matrix-benchmark))
;; => 0.532 s

;;; 3) Crazy compiler macros
;;;
;;; By parsing the environment parameter of the compiler macro, we can
;;; do some limited type inference (directly from type declarations)
;;; to optimize away the generic function call.
;;;
;;; This is not portable, because you need to ask your lisp to give
;;; you information about type declarations from the environment
;;; object, which was not standardized.  I experimented with sbcl, and
;;; I suspect that other lisps probably have constructs to do this,
;;; but it's probably not worth it right now.
;;;
;;; It works but in the expansion there is another generic function
;;; call to DATA anyway so it just halves the execution time of the
;;; benchmark in `benchmarks/lisp-vs-c-aref.lisp'.
;;;
;;; The conclusion is that VREF or MREF is not the right place to do
;;; the optimization - better have a macro 


#+sbcl
(defun lexenv-get-type (symbol env)
  (assert (typep symbol 'symbol))
  (let ((lambda-var
         (cdr (assoc symbol (sb-c::lexenv-vars env)))))
    (and lambda-var
         (typep lambda-var 'sb-c::lambda-var)
         (sb-c::lambda-var-type lambda-var))))

(defun test-type (form type env)
  "helper function for compiler macros - test that FORM is of the
  given type, either directly if it is an object, or if there is
  a (THE ...) form, or through lexical bindings in the environment
  ENV."
  (typecase form
    (list (and (eql (first form) 'the)
               (equal (second form) type)
               (not (null (third form)))))
    (symbol (let ((env-type (lexenv-get-type form env)))
              (and (subtypep env-type type)
                   (subtypep type env-type))))
    (atom (typep form type))
    (t nil)))

(define-compiler-macro vref (&whole form x i &environment env)
  ;; PRINT is there so that we can see what VREF is expanded into when
  ;; it is used in a function definition
  (cond ((test-type x 'vector-double env)
         (print
          (let ((fnv-array (gensym "FNV-ARRAY")))
           `(let ((,fnv-array (data ,x)))
              (declare (type fixnum ,i)
                       (type fnv-double ,fnv-array))
              (the double-float
                (fnv-double-ref ,fnv-array ,i))))))
        (t (print form))))

(defun lisp-matrix-vref-benchmark ()
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (dotimes (i +ASIZE+ s)
      (declare (type fixnum i))
      (incf s (vref a i)))))

#+nil
(time (lisp-matrix-vref-benchmark))
;; => 0.243 s

#||

(let ((a (make-vector 2 'double)))
  (funcall (compiler-macro-function 'vref)
           `(vref ,a 0)
           nil))

(let ((a (make-vector 2 'float)))
  (funcall (compiler-macro-function 'vref)
           `(vref ,a 0)
           nil))

(funcall (compiler-macro-function 'vref)
         `(vref ,(make-vector 2 'double) 0)
         nil)

||#

;;; Now I messed up the VREF generic, so before continuing I'll put it
;;; back.

(define-compiler-macro vref (&whole form x i)
  (declare (ignore x i))
  form)


;;; 4) higher-level macro
;;;
;;; The idea is to have a macro `with-fast-access' that inlines the
;;; calls to VREF and MREF for appropriate matrix types.  There are a
;;; few possibilities to do this: (i) evaluate the code once we know
;;; the matrix type, i.e. at runtime; (ii) include type declarations
;;; in the macro.
;;;
;;; With (ii), a call to the macro might look like this

#+nil
(let ((a (make-vector 10 'double :initial-element 1d0))
      (s 0d0))
  (with-fast-access ((a vector-double))
    (dotimes (i 10)
      (incf s (vref a i))))
  s)

;;; which would be expanded to

#+nil
(let ((a (make-vector 10 'double :initial-element 1d0))
      (s 0d0))
  (let ((DATA1 (data a)))
    (dotimes (i 10)
      (declare (type fixnum i))
      (incf s (the double-float (fnv-double-ref DATA1 i))))
    s))

;;; Here is a try at this using LABELS and MACROLET to rebind the
;;; function VREF.  This won't work for (SETF VREF), and macros
;;; WITH-FAST-ACCESS can't be nested.
;;;
;;; Note: this looks similar to what Cyrrus Harmon did in his CLEM
;;; package, who solved the problem of not working with SETF by
;;; expanding in all cases (instead of falling back to the previous
;;; VREF), and nesting by having a symbol-macrolet store the variables
;;; for which to enable fast access.
;;;
;;; A full-fledged version would use a code walker but that's another
;;; can of worms..

(defmethod vref-call ((type (eql 'vector-double)) data-var i-var)
  ``(the double-float (fnv-double-ref ,',data-var ,,i-var)))

(defmacro with-fast-access ((&rest forms) &body body)
  "Inline all calls to VREF or AREF in BODY for each vector or matrix
  in FORMS.

  This works by first binding a function to the old VREF function, and
  then defining a local macro VREF which expands into optimized code
  for each matrix in FORMS, and falls back to the function otherwise."
  (with-unique-names (a i vref)
    (loop for (var type) in forms
       for g = (gensym "DATA")
       collect `(,g (data ,var)) into data-bindings
       collect `(,var ,(vref-call type g i)) into vref-cases
       finally
       (return `(let (,@data-bindings)
                  (labels ((,vref (,a ,i) (vref ,a ,i)))
                    (declare (inline ,vref))
                    (macrolet ((vref (,a ,i)
                                 (case ,a
                                   ,@vref-cases
                                   (t `(,',vref ,,a ,,i)))))
                      ,@body)))))))

(defun lisp-matrix-fast-vref-benchmark ()
  "Here we use the macro WITH-FAST-ACCESS which inlines calls to
  VREF."
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (with-fast-access ((a vector-double))
      (dotimes (i +asize+ s)
        (declare (type fixnum i))
        (incf s (vref a i))))))

#+nil
(time (lisp-matrix-fast-vref-benchmark))
;; => 0.074 s

(defun lisp-matrix-fast-vref-benchmark2 ()
  "same as LISP-MATRIX-FAST-VREF-BENCHMARK but we `forgot' to add the
  matrix in the list of matrices to optimize fast access for and
  thereby fall back to the rebound version of VREF (macro-expand the
  macro WITH-FAST-ACCESS in this function and in
  LISP-MATRIX-FAST-VREF-BENCHMARK to see the difference)."
  (let ((a (make-vector +asize+ 'double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (with-fast-access ()
      (dotimes (i +asize+ s)
        (declare (type fixnum i))
        (incf s (vref a i))))))

#+nil
(time (lisp-matrix-fast-vref-benchmark2))
;; => 0.723 s
