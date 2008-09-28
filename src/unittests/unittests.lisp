;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lisp-matrix core package.  The dependency
;;; should be that lisp-matrix packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

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
(deftestsuite lisp-matrix-ut-vectors (lisp-matrix-ut) ())
(deftestsuite lisp-matrix-ut-matrix  (lisp-matrix-ut) ())
(deftestsuite lisp-matrix-ut-matrix-views  (lisp-matrix-ut-matrix) ())
(deftestsuite lisp-matrix-ut-matrix-lapack (lisp-matrix-ut-matrix) ())
(deftestsuite lisp-matrix-ut-matrix-gemm   (lisp-matrix-ut-matrix) ())

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
  wrong-data-initially
  (ensure-error  ;; because data is integer, not double-float!
    (let ((m1  (make-matrix 2 5
		      :implementation :lisp-array 
		      :element-type 'double-float
		      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					  (6d0 7 8 9 10)))))
      m1)))

(addtest (lisp-matrix-ut)
  right-data-initially
  (let ((n 2)
	(m 5)
	(m1 (make-matrix 2 5
			 :implementation :lisp-array 
			 :element-type 'double-float
			 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					     (6d0 7d0 8d0 9d0 10d0)))))
    (ensure (= (nrows m1) n))
    (ensure (= (ncols m1) m))
    (ensure (= (nelts m1) (* n m)))
    (ensure (= (matrix-dimension m1 0) n))
    (ensure (= (matrix-dimension m1 1) m))
    (ensure-error (matrix-dimension m1 2))
    (ensure-error (matrix-dimension m1 -1))
    (ensure (equal (matrix-dimensions m1)
		   (list n m)))))
   

;; combinations...
(addtest (lisp-matrix-ut)
  data-initialize
  (ensure-error  
    ;; because data is integer, not double-float!
    (let ((m1  (make-matrix 2 5
		      :implementation :lisp-array 
		      :element-type 'double-float
		      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					  (6d0 7 8 9 10)))))
      m1))
  (ensure
   ;; correct initial data
   (let ((m1 (make-matrix 2 5
			  :implementation :lisp-array 
			  :element-type 'double-float
			  :initial-contents '((1d0 2d0 3d0 4d0 5d0)
					      (6d0 7d0 8d0 9d0 10d0)))))
     m1)))


;; combination + progn
(addtest (lisp-matrix-ut)
  data-initialize-2
  (progn
    (ensure-error  
      ;; because data is integer, not double-float!
      (let ((m1  (make-matrix 2 5
			      :implementation :lisp-array 
			      :element-type 'double-float
			      :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						  (6d0 7 8 9 10)))))
	m1))
    (ensure
     ;; correct data input
     (let ((m1 (make-matrix 2 5
			    :implementation :lisp-array 
			    :element-type 'double-float
			    :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						(6d0 7d0 8d0 9d0 10d0)))))
       m1))))


(addtest (lisp-matrix-ut)
  silly-macro-test-1
  (ensure (silly-test 2 5)))

(addtest (lisp-matrix-ut)
  silly-macro-test-2
  (ensure (silly-test 4 4)))

(addtest (lisp-matrix-ut)
  silly-macro-test-3
  (ensure-error (silly-test 4 4)))

(addtest (lisp-matrix-ut)
  silly-macro-test-4
  (ensure (not (silly-test2 2 5))))


(addtest (lisp-matrix-ut)
  one-random-test-2
  (test-matrix-size (make-matrix 2 5
				 :implementation :lisp-array 
				 :element-type 'double-float
				 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
						     (6d0 7d0 8d0 9d0 10d0)))
		    2 5))


(addtest (lisp-matrix-ut)
 make-matrix-double-zero-size
 #-clisp (for-all-implementations
	   (ensure (make-matrix 0 0))
	   (ensure (make-matrix 0 1))
	   (ensure (make-matrix 1 0)))
 #+clisp (for-implementations (:lisp-array) ;; foriegn zero-size arrays fail in CLISP?
	   (finishes (make-matrix 0 0))
	   (finishes (make-matrix 0 1))
	   (finishes (make-matrix 1 0))))



(addtest (lisp-matrix-ut)
 transposed-p
 (for-all-implementations
    (let ((m (make-matrix 2 2)))
      (ensure (null (transposed-p m)))
      (ensure (transposed-p (transpose m)))
      (ensure (transposed-p (window (transpose m))))
      ;; the last one was removed because now the transpose of a
      ;; transpose returns the original matrix
      #+(or)
      (ensure (transposed-p (transpose (transpose m)))))))



(addtest (lisp-matrix-ut)
  m=
  (for-all-implementations
    (ensure (not (m= (make-matrix 1 2)
		     (make-matrix 1 1))))
    (ensure (not (m= (make-matrix 2 1)
		     (make-matrix 1 1))))
    (ensure (not (m= (make-matrix 1 1 :initial-element 1d0)
		     (make-matrix 1 1 :initial-element 0d0))))))


(addtest (lisp-matrix-ut) zero-offset-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (ensure (zero-offset-p m))
      (ensure (zero-offset-p (transpose m)))
      (ensure (zero-offset-p (transpose (transpose m))))
      (ensure (zero-offset-p (window m :nrows 1)))
      (ensure (zero-offset-p (strides m :ncols 1)))
      (ensure (not (zero-offset-p (window m :row-offset 1 :nrows 1))))
      (ensure (not (zero-offset-p (window m :col-offset 1 :ncols 1))))
      (ensure (not (zero-offset-p (strides m :row-offset 1 :nrows 1))))
      (ensure (not (zero-offset-p (strides m :col-offset 1 :ncols 1))))
      (ensure (not (zero-offset-p (window (strides m :col-offset 1 :ncols 1)))))
      (ensure (zero-offset-p (strides m :row-stride 2 :nrows 2))))))

(addtest (lisp-matrix-ut) unit-strides-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (ensure (unit-strides-p m))
      (ensure (unit-strides-p (transpose m)))
      (ensure (unit-strides-p (transpose (transpose m))))
      (ensure (unit-strides-p (window m :nrows 1)))
      (ensure (unit-strides-p (strides m :ncols 1)))
      (ensure (unit-strides-p (window m :row-offset 1 :nrows 1)))
      (ensure (unit-strides-p (window m :col-offset 1 :ncols 1)))
      (ensure (unit-strides-p (strides m :row-offset 1 :nrows 1)))
      (ensure (unit-strides-p (strides m :col-offset 1 :ncols 1)))
      (ensure (not (unit-strides-p (strides m :row-stride 2 :nrows 2))))
      (ensure (not (unit-strides-p (transpose (strides m :row-stride 2 :nrows 2)))))
      (ensure (not (unit-strides-p (window (strides m :row-stride 2 :nrows 2)))))
      (ensure (not (unit-strides-p (strides (strides m :row-stride 2 :nrows 2))))))))



;;;;;;;;;;;;;;;;;;;;;;;;; FIXME starts here!


;;; Matrix creation

(addtest (lisp-matrix-ut-matrix)
  ones
  (for-all-implementations
    (ensure (m= (ones 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((1.0 1.0)
                                             (1.0 1.0)))))
    (ensure (m= (ones 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((1d0 1d0)
                                             (1d0 1d0)))))
    (ensure (m= (ones 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents '((#C(1.0 0.0) #C(1.0 0.0))
                                             (#C(1.0 0.0) #C(1.0 0.0))))))
    (ensure (m= (ones 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents
                         '((#C(1d0 0d0) #C(1d0 0d0))
                           (#C(1d0 0d0) #C(1d0 0d0))))))))

(addtest (lisp-matrix-ut) zeros
  (for-all-implementations
    (ensure (m= (zeros 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((0.0 0.0)
                                             (0.0 0.0)))))
    (ensure (m= (zeros 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((0d0 0d0)
                                             (0d0 0d0)))))
    (ensure (m= (zeros 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents
                         '((#C(0.0 0.0) #C(0.0 0.0))
                           (#C(0.0 0.0) #C(0.0 0.0))))))
    (ensure (m= (zeros 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents
                         '((#C(0d0 0d0) #C(0d0 0d0))
                           (#C(0d0 0d0) #C(0d0 0d0))))))))

(addtest (lisp-matrix-ut) eye
  (for-all-implementations
    (ensure (m= (eye 2 2 :element-type 'single-float)
            (make-matrix 2 2
                         :element-type 'single-float
                         :initial-contents '((1.0 0.0)
                                             (0.0 1.0)))))
    (ensure (m= (eye 2 2 :element-type 'double-float)
            (make-matrix 2 2
                         :element-type 'double-float
                         :initial-contents '((1d0 0d0)
                                             (0d0 1d0)))))
    (ensure (m= (eye 2 2 :element-type '(complex single-float))
            (make-matrix 2 2
                         :element-type '(complex single-float)
                         :initial-contents
                         '((#C(1.0 0.0) #C(0.0 0.0))
                           (#C(0.0 0.0) #C(1.0 0.0))))))
    (ensure (m= (eye 2 2 :element-type '(complex double-float))
            (make-matrix 2 2
                         :element-type '(complex double-float)
                         :initial-contents '((#C(1d0 0d0) #C(0d0 0d0))
                                             (#C(0d0 0d0) #C(1d0 0d0))))))))

(addtest (lisp-matrix-ut) rand
  (for-all-implementations
    (let* ((state1 (make-random-state))
           (state2 (make-random-state state1)))
      (ensure (m= (rand 2 3 :state state1)
              (rand 2 3 :state state2)))
      (ensure (not (m= (rand 2 3 :state state1)
                   (rand 2 3 :state state1)))))))

;;; MATRIX-VIEWS

(addtest (lisp-matrix-ut-matrix-views)
  fun-transpose
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eq a (transpose (transpose a)))))))

(addtest (lisp-matrix-ut-matrix-views)
  fun-window
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eq a (parent (window (window a :ncols 2)
                                :nrows 2))))
      (ensure (m= (window (window a :ncols 2) :nrows 2)
              (window a :ncols 2 :nrows 2))))))

(addtest (lisp-matrix-ut-matrix-views)
  fun-strides
  (for-all-implementations
    (let ((a (rand 3 4)))
      (ensure (eql (class-name (class-of (strides a :nrows 2)))
               (window-class a)))
      (ensure (eq a (parent (strides (strides a :ncols 2 :col-stride 2))))))))

;;; VECTORS

(addtest (lisp-matrix-ut-vectors)
  construct-vectors
  (for-all-implementations
    (ensure (m= (make-vector 3 :initial-element 0d0)
            (make-matrix 1 3 :initial-element 0d0)))
    (ensure (m= (make-vector 3 :initial-element 0d0 :type :column)
            (make-matrix 3 1 :initial-element 0d0)))
    (ensure (col-vector-p (rand 3 1)))
    (ensure (row-vector-p (rand 1 3)))
    (let ((a (rand 3 5)))
      (ensure (v= (row a 0) (col (transpose a) 0)))
      (ensure (not (m= (row a 0) (col (transpose a) 0))))
      (ensure (row-vector-p (row a 0)))
      (ensure (col-vector-p (col a 0)))
      (ensure (row-vector-p (row (transpose a) 0)))
      (ensure (col-vector-p (col (transpose a) 0)))
      ;; strides and window should return vectors when appropriate
      (ensure (row-vector-p (window a :nrows 1)))
      (ensure (col-vector-p (window a :ncols 1)))
      ;; transpose should return the original matrix if dimensions are
      ;; 1 x 1
      (let ((m (rand 1 1)))
        (ensure (eq m (transpose m))))
      ;; FIXME: M x 1 or 1 x M matrices should not be considered
      ;; transposed when we think of their storage.  But we cannot
      ;; transpose them without resorting to a TRANSPOSE-VECVIEW.  So
      ;; it would be best to introduce a function like
      ;; STORAGE-TRANSPOSED-P.
      ;; (ensure (not (transposed-p (transpose (make-matrix 1 10)))))
      ;; (ensure (not (transposed-p (transpose (make-matrix 10 1)))))
      )))

(addtest (lisp-matrix-ut-vectors)
  row-of-strided-matrix
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 2 :row-stride 2)))
    (ensure (m= (row b 0)
            (make-matrix 1 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)))))
    (ensure (m= (row b 1)
            (make-matrix 1 5 :initial-contents '((11d0 12d0 13d0 14d0 15d0)))))))

(addtest (lisp-matrix-ut-vectors)
  col-of-strided-matrix
  (let* ((a (make-matrix 6 5 :initial-contents '((1d0 2d0 3d0 4d0 5d0)
                                                 (6d0  7d0  8d0  9d0  10d0)
                                                 (11d0 12d0 13d0 14d0 15d0)
                                                 (16d0 17d0 18d0 19d0 20d0)
                                                 (21d0 22d0 23d0 24d0 25d0)
                                                 (26d0 27d0 28d0 29d0 30d0))))
         (b (strides a :nrows 2 :row-stride 2)))
    (ensure (m= (col b 0)
            (make-matrix 2 1 :initial-contents '((1d0) (11d0)))))
    (ensure (m= (col b 1)
            (make-matrix 2 1 :initial-contents '((2d0) (12d0)))))
    (ensure (m= (col b 2)
            (make-matrix 2 1 :initial-contents '((3d0) (13d0)))))
    (ensure (m= (col b 3)
            (make-matrix 2 1 :initial-contents '((4d0) (14d0)))))
    (ensure (m= (col b 4)
            (make-matrix 2 1 :initial-contents '((5d0) (15d0)))))))

(addtest (lisp-matrix-ut-vectors)
  v=
  (let ((a (rand 3 4)))
    ;; FIXME: this also tests ROW, COL, and their use on a transposed
    ;; matrix
    (ensure (v= (row a 0) (col (transpose a) 0)))
    (ensure (v= (col a 0) (row (transpose a) 0)))))

(addtest (lisp-matrix-ut-vectors)
  row-of-window
  (let* ((a (rand 5 10 :element-type 'integer :value 10))
         (b (window a :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 0)
            (window a :row-offset 1 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 1)
            (window a :row-offset 2 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 2)
            (window a :row-offset 3 :nrows 1 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 3)
            (window a :row-offset 4 :nrows 1 :col-offset 2 :ncols 5))))
  (let* ((a (rand 10 5 :element-type 'integer :value 10))
         (b (window (transpose a) :row-offset 1 :nrows 4 :col-offset 2 :ncols 5)))
    (ensure (m= (row b 0)
            (window (transpose a) :row-offset 1 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 1)
            (window (transpose a) :row-offset 2 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 2)
            (window (transpose a) :row-offset 3 :nrows 1 :col-offset 2
                                                         :ncols 5)))
    (ensure (m= (row b 3)
            (window (transpose a) :row-offset 4 :nrows 1 :col-offset 2
                                                         :ncols 5)))))

(addtest (lisp-matrix-ut-vectors)
  real-stride
  (ensure (= 1 (real-stride (zeros 2 2))))
  (ensure (= 2 (real-stride (row (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (col (zeros 2 2) 0))))
  (ensure (= 1 (real-stride (row (transpose (zeros 2 2)) 0))))
  (ensure (= 2 (real-stride (col (transpose (zeros 2 2)) 0))))
  (ensure (null (real-stride (window (zeros 4 4) :nrows 2)))))


;;; Test lapack


(addtest (lisp-matrix-ut-matrix-lapack)
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
  (ensure (equal (make-predicate 't)
             '(constantly t)))
  (ensure (equal (make-predicate 'nil)
             '(constantly nil))))

(addtest (lisp-matrix-ut)
  datatypes
  (ensure (string= (datatype->letter 'float) "S"))
  (ensure (string= (datatype->letter 'double) "D"))
  (ensure (string= (datatype->letter 'complex-float) "C"))
  (ensure (string= (datatype->letter 'complex-double) "Z")))

;; FIXME: tests below up to IAMAX fail on SBCL versions before and
;; including 1.0.11, but succeed after and including 1.0.12

(addtest (lisp-matrix-ut)
  scal
  (for-all-implementations
    (ensure
     (m=
      (scal 1.5d0 (ones 2 2 :element-type 'double-float))
      (make-matrix 2 2 :element-type 'double-float
                       :initial-element 1.5d0)))
    (ensure
     (m=
      (scal 1.5 (ones 2 2 :element-type 'single-float))
      (make-matrix 2 2 :element-type 'single-float
                       :initial-element 1.5)))
    (ensure
     (m=
      (scal #C(1.5 1.5)
            (ones 2 2 :element-type '(complex single-float)))
      (make-matrix 2 2 :element-type '(complex single-float)
                       :initial-element #C(1.5 1.5))))
    (ensure
     (m=
      (scal #C(1.5d0 1.5d0)
            (ones 2 2 :element-type '(complex double-float)))
      (make-matrix 2 2 :element-type '(complex double-float)
                       :initial-element #C(1.5d0 1.5d0))))))

(addtest (lisp-matrix-ut) axpy
  (for-all-implementations
    (let ((*default-element-type* 'single-float))
      (ensure (m= (axpy 1.0 (ones 2 2) (scal 1.5 (ones 2 2)))
              (scal 2.5 (ones 2 2))))
      (ensure (m= (axpy -1.0 (ones 2 2) (scal 1.5 (ones 2 2)))
              (scal 0.5 (ones 2 2)))))
    (let ((*default-element-type* 'double-float))
      (ensure (m= (axpy 1d0 (ones 2 2) (scal 1.5d0 (ones 2 2)))
              (scal 2.5d0 (ones 2 2))))
      (ensure (m= (axpy -1d0 (ones 2 2) (scal 1.5d0 (ones 2 2)))
              (scal 0.5d0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float)))
      (ensure (m= (axpy #C(1.0 0.0)
                    (ones 2 2)
                    (scal #C(1.5 0.0) (ones 2 2)))
              (scal #C(2.5 0.0) (ones 2 2))))
      (ensure (m= (axpy #C(-1.0 0.0)
                    (ones 2 2)
                    (scal #C(1.5 0.0) (ones 2 2)))
              (scal #C(0.5 0.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float)))
      (ensure (m= (axpy #C(1.0d0 0.0d0)
                    (ones 2 2)
                    (scal #C(1.5d0 0.0d0) (ones 2 2)))
              (scal #C(2.5d0 0.0d0) (ones 2 2))))
      (ensure (m= (axpy #C(-1.0d0 0.0d0)
                    (ones 2 2)
                    (scal #C(1.5d0 0.0d0) (ones 2 2)))
              (scal #C(0.5d0 0.0d0) (ones 2 2)))))))


(addtest (lisp-matrix-ut-matrix) dot
  (for-all-implementations
    (ensure (= (dot (ones 2 2)
		    (scal 0.5d0 (ones 2 2)))
	       2d0))
    (ensure (= (dot (ones 2 2 :element-type 'single-float)
		    (scal 0.5 (ones 2 2 :element-type 'single-float)))
	       2.0))))

#|
;; FIXME: test DOTU, DOTC
#+(or)
(addtest (lisp-matrix-ut) dotu
 (ensure (= (dotu (ones 2 2 :element-type '(complex single-float))
              (scal #C(0.5 0.0) (ones 2 2 :element-type
                                      '(complex single-float))))
        #C(2.0 0.0))))

#+(or)
(addtest (lisp-matrix-ut) dotc
 (ensure (= (dotc (ones 2 2 :element-type '(complex single-float))
              (scal #C(0.5 0.0) (ones 2 2 :element-type
                                      '(complex single-float))))
        #C(2.0 0.0))))
|#

(addtest (lisp-matrix-ut) nrm2
  (for-all-implementations
    (ensure (= (nrm2 (ones 2 2))
           2d0))
    (ensure (= (nrm2 (ones 2 2 :element-type 'single-float))
           2.0))
    (ensure (= (nrm2 (ones 2 2 :element-type '(complex single-float)))
           #C(2.0 0.0)))
    (ensure (= (nrm2 (ones 2 2 :element-type '(complex double-float)))
           #C(2d0 0d0)))))

(addtest (lisp-matrix-ut) asum
  (for-all-implementations
    (ensure (= (asum (ones 2 2))
           4d0))
    (ensure (= (asum (ones 2 2 :element-type 'single-float))
           4.0))
    (ensure (= (asum (ones 2 2 :element-type '(complex single-float)))
           #C(4.0 0.0)))
    (ensure (= (asum (ones 2 2 :element-type '(complex double-float)))
           #C(4d0 0d0)))))

(addtest (lisp-matrix-ut) iamax
  (for-all-implementations
    (ensure (= (iamax (make-matrix 2 2 :initial-contents '((1d0 2d0)
                                                       (1d0 1d0))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type 'single-float
                                   :initial-contents '((1.0 2.0)
                                                       (1.0 1.0))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type '(complex single-float)
                                   :initial-contents '((#C(1.0 0.0) #C(2.0 0.0))
                                                       (#C(1.0 0.0) #C(1.0 0.0)))))
           2))
    (ensure (= (iamax (make-matrix 2 2 :element-type '(complex double-float)
                                   :initial-contents '((#C(1d0 0d0) #C(2d0 0d0))
                                                       (#C(1d0 0d0) #C(1d0 0d0)))))
           2))
    (ensure (= (iamax (ones 1 1))
           0))
    (ensure (= (iamax (ones 1 1 :element-type 'single-float))
           0))
    (ensure (= (iamax (ones 1 1 :element-type '(complex single-float)))
           0))
    (ensure (= (iamax (ones 1 1 :element-type '(complex double-float)))
           0))))


;;;; GEMM tests


(defun check-m* (a b)
  (let ((result (make-matrix 2 2 :initial-contents
                             '((19d0 22d0)
                               (43d0 50d0)))))
    (ensure (m= result (m* a b)))))

(defmacro def-m*-test (name a b)
  `(addtest (lisp-matrix-ut-matrix-gemm) ,name
     (for-all-implementations
       (check-m* ,a ,b))))

(def-m*-test m*-basic-test
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-a
    (transpose
     (make-matrix 2 2 :initial-contents
                  '((1d0 3d0)
                    (2d0 4d0))))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-b
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (transpose
   (make-matrix 2 2 :initial-contents
                '((5d0 7d0)
                  (6d0 8d0)))))

(def-m*-test m*-double-transpose-a
    (transpose
     (transpose
      (make-matrix 2 2 :initial-contents
                   '((1d0 2d0)
                     (3d0 4d0)))))
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-transpose-a-b
    (transpose
     (make-matrix 2 2 :initial-contents
                  '((1d0 3d0)
                    (2d0 4d0))))
  (transpose
   (make-matrix 2 2 :initial-contents
                '((5d0 7d0)
                  (6d0 8d0)))))

(def-m*-test m*-window-a-nocopy
    (window
     (make-matrix 3 3 :initial-contents
                  '((1d0 2d0 0d0)
                    (3d0 4d0 0d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-window-a-copy
    (window
     (make-matrix 3 3 :initial-contents
                  '((0d0 1d0 2d0)
                    (0d0 3d0 4d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2 :col-offset 1)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-window-b-nocopy
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (window
   (make-matrix 2 3 :initial-contents
                '((5d0 6d0 0d0)
                  (7d0 8d0 0d0)))
   :ncols 2))

(def-m*-test m*-window-b-copy
    (make-matrix 2 2 :initial-contents
                 '((1d0 2d0)
                   (3d0 4d0)))
  (window
   (make-matrix 3 3 :initial-contents
                '((0d0 0d0 0d0)
                  (5d0 6d0 0d0)
                  (7d0 8d0 0d0)))
   :ncols 2 :nrows 2 :row-offset 1))

(def-m*-test m*-stride-a-nocopy
    (strides
     (make-matrix 3 3 :initial-contents
                  '((1d0 2d0 0d0)
                    (3d0 4d0 0d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(def-m*-test m*-stride-a-copy
    (strides
     (make-matrix 4 3 :initial-contents
                  '((1d0 0d0 2d0)
                    (0d0 0d0 0d0)
                    (3d0 0d0 4d0)
                    (0d0 0d0 0d0)))
     :nrows 2 :ncols 2 :row-stride 2 :col-stride 2)
  (make-matrix 2 2 :initial-contents
               '((5d0 6d0)
                 (7d0 8d0))))

(addtest (lisp-matrix-ut-matrix-gemm)
  gemm-window-c-copy
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 3 3))
           (windowed-c (window c :nrows 2 :ncols 2)))
      (ensure (eq windowed-c
              (gemm 1d0
                    (make-matrix 2 2 :initial-contents
                                 '((1d0 2d0)
                                   (3d0 4d0)))
                    (make-matrix 2 2 :initial-contents
                                 '((5d0 6d0)
                                   (7d0 8d0)))
                    0d0
                    windowed-c)))
      (ensure (m= windowed-c result))
      (ensure (m= windowed-c (window c :nrows 2 :ncols 2)))
      (ensure (m= (window c :nrows 1 :row-offset 2)
              (zeros 1 3)))
      (ensure (m= (window c :ncols 1 :col-offset 2)
              (zeros 3 1))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  gemm-window-c-copy-copyback
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 4 4))
           (windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
                                                   :col-offset 2)))
      (ensure (eq windowed-c
              (gemm 1d0
                    (make-matrix 2 2 :initial-contents
                                 '((1d0 2d0)
                                   (3d0 4d0)))
                    (make-matrix 2 2 :initial-contents
                                 '((5d0 6d0)
                                   (7d0 8d0)))
                    0d0
                    windowed-c)))
      (ensure (m= windowed-c result))
      (ensure (m= windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
                                                     :col-offset 2)))
      (ensure (m= (window c :nrows 2) (zeros 2 4)))
      (ensure (m= (window c :ncols 2) (zeros 4 2))))))


(addtest (lisp-matrix-ut-matrix-gemm)
  m*-double
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type 'double-float
                            :initial-contents '((1d0 2d0 0d0)
                                                (3d0 4d0 0d0)
                                                (0d0 0d0 0d0)))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type 'double-float
                           :initial-contents '((5d0 6d0)
                                               (7d0 8d0))))
      (make-matrix 2 2 :element-type 'double-float
                       :initial-contents '((19d0 22d0)
                                           (43d0 50d0)))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-single
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type 'single-float
                            :initial-contents '((1.0 2.0 0.0)
                                                (3.0 4.0 0.0)
                                                (0.0 0.0 0.0)))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type 'single-float
                           :initial-contents '((5.0 6.0)
                                               (7.0 8.0))))
      (make-matrix 2 2 :element-type 'single-float
                       :initial-contents '((19.0 22.0)
                                           (43.0 50.0)))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-complex-single
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type '(complex single-float)
                            :initial-contents
                            '((#C(1.0 0.0) #C(2.0 0.0) #C(0.0 0.0))
                              (#C(3.0 0.0) #C(4.0 0.0) #C(0.0 0.0))
                              (#C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0))))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type '(complex single-float)
                           :initial-contents
                           '((#C(5.0 0.0) #C(6.0 0.0))
                             (#C(7.0 0.0) #C(8.0 0.0)))))
      (make-matrix 2 2 :element-type '(complex single-float)
                       :initial-contents '((#C(19.0 0.0) #C(22.0 0.0))
                                           (#C(43.0 0.0) #C(50.0 0.0))))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-complex-double
  (for-all-implementations
    (ensure
     (m=
      (m* (window
           (make-matrix 3 3 :element-type '(complex double-float)
                            :initial-contents
                            '((#C(1d0 0d0) #C(2d0 0d0) #C(0d0 0d0))
                              (#C(3d0 0d0) #C(4d0 0d0) #C(0d0 0d0))
                              (#C(0d0 0d0) #C(0d0 0d0) #C(0d0 0d0))))
           :nrows 2 :ncols 2)
          (make-matrix 2 2 :element-type '(complex double-float)
                           :initial-contents
                           '((#C(5d0 0d0) #C(6d0 0d0))
                             (#C(7d0 0d0) #C(8d0 0d0)))))
      (make-matrix 2 2 :element-type '(complex double-float)
                       :initial-contents '((#C(19d0 0d0) #C(22d0 0d0))
                                           (#C(43d0 0d0) #C(50d0
  0d0))))))))

(addtest (lisp-matrix-ut-matrix-gemm)
  m*-vectors
  (for-all-implementations
    (let* ((a (make-matrix 4 4 :initial-contents '((0d0 1d0 2d0 3d0)
                                                   (1d0 2d0 3d0 4d0)
                                                   (2d0 3d0 4d0 5d0)
                                                   (3d0 4d0 5d0 6d0))))
           (x (slice (col a 3) :stride 2 :nelts 2 :type :row))
           (y (slice (col a 2) :stride 2 :nelts 2 :type :column)))
      (ensure (m= x (make-matrix 1 2 :initial-contents '((3d0 5d0)))))
      (ensure (m= y (make-matrix 2 1 :initial-contents '((2d0) (4d0)))))
      (ensure (m= (m* x y) (scal 26d0 (ones 1 1))))
      (ensure (m= (m* y x) (make-matrix 2 2 :initial-contents '((6d0 10d0)
                                                         (12d0 20d0))))))
    (ensure (m= (m* (ones 1 10) (ones 10 1))
            (scal 10d0 (ones 1 1))))
    (ensure (m= (m* (ones 10 1)
                (scal 2d0 (ones 1 10)))
            (scal 2d0 (ones 10 10))))))




(addtest (lisp-matrix-ut-matrix-lapack)
  m+
  (for-all-implementations
    (let* ((a (ones 2 2))
           (b (scal 2d0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2d0 (ones 2 2))))
      (ensure (m= (m+ a b) (scal 3d0 (ones 2 2)))))
    (let* ((*default-element-type* 'single-float)
           (a (ones 2 2))
           (b (scal 2.0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2.0 (ones 2 2))))
      (ensure (m= (m+ a b) (scal 3.0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float))
           (a (ones 2 2))
           (b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= (m+ a b) (scal #C(3.0 2.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float))
           (a (ones 2 2))
           (b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= (m+ a b) (scal #C(3d0 2d0) (ones 2 2)))))))

(addtest (lisp-matrix-ut-matrix-lapack)
  m-
  (for-all-implementations
    (let* ((a (ones 2 2))
           (b (scal 2d0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2d0 (ones 2 2))))
      (ensure (m= (m- a b) (scal -1d0 (ones 2 2)))))
    (let* ((*default-element-type* 'single-float)
           (a (ones 2 2))
           (b (scal 2.0 (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal 2.0 (ones 2 2))))
      (ensure (m= (m- a b) (scal -1.0 (ones 2 2)))))
    (let* ((*default-element-type* '(complex single-float))
           (a (ones 2 2))
           (b (scal #C(2.0 2.0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2.0 2.0) (ones 2 2)))) 
      (ensure (m= (m- a b) (scal #C(-1.0 -2.0) (ones 2 2)))))
    (let* ((*default-element-type* '(complex double-float))
           (a (ones 2 2))
           (b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= a (ones 2 2)))
      (ensure (m= b (scal #C(2d0 2d0) (ones 2 2))))
      (ensure (m= (m- a b) (scal #C(-1d0 -2d0) (ones 2 2)))))))



;;;;;;;;; NOT IN FIX PROCESS FIXME.
#|

(addtest (lisp-matrix-ut) copy
  (for-all-implementations
    (labels ((test-copy-m= (a b)
               (and (not (eq a b))
                    (m= a b)))
             (test-copy (a)
               (let ((b (copy a))
                     (c (make-matrix (nrows a) (ncols a)
                                     :element-type (element-type a)
                                     :implementation (implementation a))))
                 (finishes (copy! a c))
                 (ensure (test-copy-m= a b))
                 (ensure (test-copy-m= b c))
                 (ensure (test-copy-m= a c)))))
      (for-all ((n (gen-integer :min 0 :max 10) #+clisp (> n 0))
                (m (gen-integer :min 0 :max 10) #+clisp (> m 0))
                (n2 (gen-integer :min 0 :max 10)
                    (and (<= n2 n) #+clisp (> n2 0)))
                (m2 (gen-integer :min 0 :max 10)
                    (and (<= m2 m) #+clisp (> m2 0)))
                (row-offset (gen-integer :min 0 :max 10)
                            (<= row-offset (- n n2)))
                (col-offset (gen-integer :min 0 :max 10)
                            (<= col-offset (- m m2))))
        (test-copy (rand n m))
        (test-copy (transpose (rand n m)))
        (test-copy (window (rand n m)
                           :nrows n2 :ncols m2
                           :row-offset row-offset
                           :col-offset col-offset))))))

(addtest (lisp-matrix-ut)
  m=-2
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 10))
              (m (gen-integer :min 1 :max 10)))
      (let ((a (rand n m)))
        (ensure
         (m= (make-matrix n m :initial-contents a)
             (make-matrix n m :initial-contents a)))))))


(addtest (lisp-matrix-ut)
  setf-mref
  (for-all-implementations
    (for-all ((n (gen-integer :min 0 :max 10) #+clisp (> n 0))
              (m (gen-integer :min 0 :max 10) #+clisp (> m 0)))
      (let ((a (make-matrix n m))
            (b (rand n m)))    
        (finishes
          (dotimes (i n)
            (dotimes (j m)
              (setf (mref a i j) (mref b i j)))))
        (ensure (m= a b))))))


;;; THESE NEED SERIOUS WORK!  (I.e. I need more brainpower to recast,
;;; but it's useful for me...)


(addtest (lisp-matrix-ut) make-matrix-double-1
  "default initial value"
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 100))
              (m (gen-integer :min 1 :max 100)))
      (let (matrix)
        (finishes (setq matrix (make-matrix n m)))
        (test-matrix-size matrix n m)
        (dotimes (i n)
          (dotimes (j m)
            (unless (typep (mref matrix i j) 'double-float)
              (fail "Element (~d,~d) of matrix ~A is not of type ~
                    DOUBLE-FLOAT"
                    i j matrix))))))))

(addtest (lisp-matrix-ut) make-matrix-double-2
  "initial value to 1d0"
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 100))
              (m (gen-integer :min 1 :max 100)))
      (let (matrix)
        (finishes (setq matrix (make-matrix n m :initial-element 1d0)))
        (test-matrix-size matrix n m)
        (dotimes (i n)
          (dotimes (j m)
            (unless (= (mref matrix i j) 1d0)
              (fail "(mref matrix ~d ~d) is ~a, should be ~a"
                    i j (mref matrix i j) 1d0))))))))

(addtest (lisp-matrix-ut) make-matrix-double-3
  "set initial contents"
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 100))
              (m (gen-integer :min 1 :max 100)))
      (let ((array (random-array n m))	  
            matrix)
        (finishes (setq matrix (make-matrix n m :initial-contents
                                            array)))
        (test-matrix-size matrix n m)
        (dotimes (i n)
          (dotimes (j m)
            (unless (= (mref matrix i j) (aref array i j))
              (fail "(mref matrix ~d ~d) is ~a, should be ~a"
                    i j (mref matrix i j) (aref array i j)))))))))

(addtest (lisp-matrix-ut) make-matrix-double-4
  "set initial contents from a list"
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 100))
              (m (gen-integer :min 1 :max 100)))
      (let* ((list (loop repeat n collect
                         (loop repeat m collect (random 1d0))))
             (matrix1 (make-matrix n m
                                   :initial-contents
                                   (make-array (list n m)
                                               :initial-contents
                                               list)))
             matrix2)
        (finishes (setq matrix2
                        (make-matrix n m :initial-contents
                                     list)))
        (test-matrix-size matrix2 n m)
        (dotimes (i n)
          (dotimes (j m)
            (unless (= (mref matrix2 i j) (mref matrix1 i j))
              (fail "(mref matrix2 ~d ~d) is ~a, should be ~a"
                    i j (mref matrix2 i j) (mref matrix1 i j)))))))))

(addtest (lisp-matrix-ut) transpose-double
  (for-all-implementations
    (for-all ((n (gen-integer :min 0 :max 100) #+clisp (> n 0))
              (m (gen-integer :min 0 :max 100) #+clisp (> m 0)))
      (let ((matrix1 (rand n m))
            matrix2 matrix3)
        (finishes (setq matrix2 (transpose matrix1)))
        (finishes (setq matrix3 (transpose matrix2)))
        (test-matrix-size matrix2 m n)
        (test-matrix-size matrix3 n m)
        (dotimes (i n)
          (dotimes (j m)
            (unless (= (mref matrix2 j i) (mref matrix1 i j))
              (fail "(mref matrix2 ~d ~d) is ~a, should be ~a"
                    i j (mref matrix2 j i) (mref matrix1 i j)))
            (unless (= (mref matrix3 i j) (mref matrix1 i j))
              (fail "(mref matrix3 ~d ~d) is ~a, should be ~a"
                    i j (mref matrix3 i j) (mref matrix1 i j)))))))))

(addtest (lisp-matrix-ut) window-double
  (for-all-implementations
    (for-all ((n (gen-integer :min 0 :max 100) #+clisp (> n 0))
              (m (gen-integer :min 0 :max 100) #+clisp (> m 0))
              (n2 (gen-integer :min 0 :max 100) (<= n2 n) #+clisp (> n2 0))
              (m2 (gen-integer :min 0 :max 100) (<= m2 m) #+clisp (> m2 0))
              (row-offset (gen-integer :min 0 :max 100)
                          (<= row-offset (- n n2)))
              (col-offset (gen-integer :min 0 :max 100)
                          (<= col-offset (- m m2))))
      (let ((matrix1 (make-matrix n m :initial-contents
                                  (random-array n m)))
            matrix2)
        (finishes (setq matrix2 (window matrix1 :nrows n2 :ncols m2
                                        :row-offset row-offset
                                        :col-offset col-offset)))
        (test-matrix-size matrix2 n2 m2)
        (dotimes (i n2)
          (dotimes (j m2)
            (unless (= (mref matrix1 (+ i row-offset) (+ j col-offset))
                       (mref matrix2 i j))
              (fail "(mref matrix2 ~d ~d) is ~a, should be ~a"
                    i j (mref matrix1 (+ i row-offset) (+ j col-offset))
                    (mref matrix2 i j)))))))))

|#
