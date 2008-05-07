(in-package :lisp-matrix)

(import 'cl-utilities:once-only)

(defmethod vref-code ((a matrix-double) i)
  (once-only (a i)
   `((declare (type fixnum ,i))
     (fnv-double-ref (data ,a) ,i))))

(defmethod vref-lambda ((a matrix-double))
  (lambda (a i)
    (declare (type fixnum i))
    (fnv-double-ref (data a) i)))

(defmethod vref-lambda ((a vector-double))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (lambda (a i)
    (declare (type fixnum i)
             (type vector-double a))
    (the double-float (fnv-double-ref (data a) i))))

(defmethod mapc-matrix (function (a vector-double))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (i (nelts a))
    (declare (type fixnum i))
    (funcall function (fnv-double-ref (data a) i))))

(defun cube (x) (* x x x))

(define-compiler-macro cube (x)
  (if (numberp x)
      (* x x x)
      `(let ((.x ,x))
         (* .x .x .x))))

(funcall (compiler-macro-function 'cube)
         '(cube (incf x))
         nil)

(define-compiler-macro mref (&whole form a i j)
  (typecase a
    (matrix-double
     `(progn
        (declare (type fixnum ,i ,j))
        (the double-float
          (fnv-double-ref (data ,a)
                          (flatten-matrix-indices ,a ,i ,j)))))
    (t
     form)))


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
given type, either directly if it is an object, or if there is a (THE
...) form, or through lexical bindings in the environment ENV."
  (typecase form
    (list (and (eql (first form) 'the)
               (equal (second form) type)
               (not (null (third form)))))
    (atom
     (or (typep form type)
         (let ((env-type (lexenv-get-type form env)))
           (and (subtypep env-type type)
                (subtypep type env-type)))))
    (t nil)))

(the double-float 1d0)

(define-compiler-macro vref (&whole form x i &environment env)
  (cond ((test-type x 'vector-double env)
         (print
          (let ((fnv-array (gensym "FNV-ARRAY")))
           `(let ((,fnv-array (data ,x)))
              (declare (type fixnum ,i)
                       (type fnv-double ,fnv-array))
              (the double-float
                (fnv-double-ref ,fnv-array ,i))))))
        (t (print form))))

(let ((a (make-matrix 2 2 'double)))
  (funcall (compiler-macro-function 'mref)
           `(mref ,a 0 0)
           nil))

(let ((a (make-matrix 2 2 'float)))
  (funcall (compiler-macro-function 'mref)
           `(mref ,a 0 0)
           nil))

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

(let ((a (make-matrix 2 2 'float)))
  (mref a 1 1))

