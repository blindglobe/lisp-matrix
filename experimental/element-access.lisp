;;; Time-stamp: <2008-05-19 17:21:16 Evan Monroig>
;;; 
;;; Here I made several experiments to see how to improve vector and
;;; matrix element access.
;;;
;;;

(in-package :lisp-matrix)

(import 'cl-utilities:once-only)

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
