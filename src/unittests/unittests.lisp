;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lisp-matrix core package.  The dependency
;;; should be that lisp-matrix packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

;;; This file contains top level structures and any general purpose
;;; support functions.   More than this would be driven by top-level
;;; testing, not matrix/vector computation, which needf to be in sub
;;; functions. 

;; (asdf:oos 'asdf:compile-op 'lift :force t)o
;; (asdf:oos 'asdf:load-op 'lift)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

;;(run-lisp-matrix-tests)
;;(describe (run-lisp-matrix-tests))

;;(remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

;;; EXTERNAL

(defun run-lisp-matrix-tests ()
  "Check everything...!"
  (run-tests :suite 'lisp-matrix-ut))

;;(defun run-lisp-matrix-test (&rest x) (run-test x))

;;; TEST SUITES

(deftestsuite lisp-matrix-ut () ())

;;; SUPPORT FUNCTIONS

(defun random-array (n m)
  "Return a random 2D array of size N x M.  Useful as input into a
make-matrix initial contents, reproducible if we set seed initially."
  (make-array (list n m)
              :element-type 'double-float
              :initial-contents
              (loop for i below n collect
                    (loop for j below m collect
                          (random 1d0)))))
;; (random-array 2 3)

(defmacro test-matrix-size (matrix n m)
  "test all size functions of MATRIX against N and M"
  `(progn
     (ensure (= (nrows ,matrix) ,n))
     (ensure (= (ncols ,matrix) ,m))
     (ensure (= (nelts ,matrix) (* ,n ,m)))
     (ensure (= (matrix-dimension ,matrix 0) ,n))
     (ensure (= (matrix-dimension ,matrix 1) ,m))
     (ensure-error (matrix-dimension ,matrix 2))
     (ensure-error (matrix-dimension ,matrix -1))
     (ensure (equal (matrix-dimensions ,matrix)
		    (list ,n ,m)))))


;;(test-matrix-size (make-matrix 2 5
;; 			       :implementation :lisp-array 
;; 			       :element-type 'double-float
;; 			       :initial-contents '((1d0 2d0 3d0 4d0 5d0)
;; 						   (6d0 7d0 8d0 9d0 10d0)))
;; 		  2 5)


(defmacro for-implementations ((&rest implementations) &body body)
  "Execute BODY for each implementation in IMPLEMENTATIONS."
  `(progn
     ,@(loop for implementation in implementations collect
             `(let ((*default-implementation* ,implementation)
                    (*default-element-type* 'double-float))
                ,@body))))

(defmacro for-all-implementations (&body body)
  `(for-implementations ,(mapcar #'car *implementations*)
     ,@body))


;; from fiveam
(defun gen-integer (&key (max (1+ most-positive-fixnum))
                         (min (1- most-negative-fixnum)))
  "Returns a generator which produces random integers greater
than or equal to MIN and less than or equal to MIN."
  (lambda ()
    (+ min (random (1+ (- max min))))))

;; from fiveam
#|
(defmacro for-all (bindings &body body)
  "Bind BINDINGS to random variables and test BODY *num-trials* times.

BINDINGS is a list of binding forms, each element is a list
of (BINDING VALUE &optional GUARD). Value, which is evaluated
once when the for-all is evaluated, must return a generator which
be called each time BODY is evaluated. BINDING is either a symbol
or a list which will be passed to destructuring-bind. GUARD is a
form which, if present, stops BODY from executing when IT returns
NIL. The GUARDS are evaluated after all the random data has been
generated and they can refer to the current value of any
binding. NB: Generator forms, unlike guard forms, can not contain
references to the boud variables.

Examples:

  (for-all ((a (gen-integer)))
    (is (integerp a)))

  (for-all ((a (gen-integer) (plusp a)))
    (is (integerp a))
    (is (plusp a)))

  (for-all ((less (gen-integer))
            (more (gen-integer) (< less more)))
    (is (<= less more)))

  (for-all (((a b) (gen-two-integers)))
    (is (integerp a))
    (is (integerp b)))"
  (with-unique-names (test-lambda-args)
    `(perform-random-testing
      (list ,@(mapcar #'second bindings))
      (lambda (,test-lambda-args)
        (destructuring-bind ,(mapcar #'first bindings)
            ,test-lambda-args
          (if (and ,@(delete-if #'null (mapcar #'third bindings)))
              (progn ,@body)
              (throw 'run-once
                (list :guard-conditions-failed))))))))
|#


;; From ARNESI

(defmacro with-unique-names ((&rest bindings) &body body)
  "Evaluate BODY with BINDINGS bound to fresh unique symbols.

Syntax: WITH-UNIQUE-NAMES ( [ var | (var x) ]* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar (lambda (binding)
                   (check-type binding (or cons symbol))
                   (destructuring-bind (var &optional (prefix (symbol-name var)))
                       (if (consp binding) binding (list binding))
                     (check-type var symbol)
                     `(,var (gensym ,(concatenate 'string prefix "-")))))
                 bindings)
     ,@body))


;; macro within a macro

(defmacro silly-test (b2 b5)
  `(let ((m1 (make-matrix ,b2 ,b5
			   :implementation :lisp-array 
			   :element-type 'double-float
			   :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					       (6d0 7d0 8d0 9d0 10d0)))))

      m1))


(defmacro silly-test2 (b2 b5)
  `(not (let ((m1 (make-matrix ,b2 ,b5
			       :implementation :lisp-array 
			       :element-type 'double-float
			       :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						   (6d0 7d0 8d0 9d0 10d0)))))
	  m1)))



;;; TESTS

(addtest (lisp-matrix-ut)
  make-predicate
  (ensure (equal (make-predicate 'unit-strides-p)
             'unit-strides-p))
  (ensure (equal (make-predicate '(not unit-strides-p))
             '(lambda (a)
               (not (unit-strides-p a)))))
  (ensure (equal (make-predicate '(or (not unit-strides-p)
                               (not zero-offset-p)))
             '(lambda (a)
               (or (not (unit-strides-p a))
                (not (zero-offset-p a))))))
  (ensure (equal (make-predicate '(or (not unit-strides-p)
                               (not zero-offset-p)
                               transposed-p))
             '(lambda (a)
               (or (not (unit-strides-p a))
                (not (zero-offset-p a))
                (transposed-p a)))))
#| 


|#

  (ensure (equal (make-predicate 't)
             '(constantly t)))
  (ensure (equal (make-predicate 'nil)
             '(constantly nil))))



#|
(addtest (lisp-matrix-ut-matrix-lapack)
  make-predicate-macro
  (ensure (equal (make-predicate-macro 'unit-strides-p)
             'unit-strides-p))
  (ensure (equal (make-predicate-macro '(not unit-strides-p))
             '(lambda (a)
               (not (unit-strides-p a)))))
  (ensure (equal (make-predicate-macro '(or (not unit-strides-p)
                               (not zero-offset-p)))
             '(lambda (a)
               (or (not (unit-strides-p a))
                (not (zero-offset-p a))))))
  (ensure (equal (make-predicate-macro '(or (not unit-strides-p)
                               (not zero-offset-p)
                               transposed-p))
             '(lambda (a)
               (or (not (unit-strides-p a))
                (not (zero-offset-p a))
                (transposed-p a)))))
  (ensure (equal (make-predicate-macro 't)
             '(constantly t)))
  (ensure (equal (make-predicate-macro 'nil)
             '(constantly nil))))
|#


