(in-package :lisp-matrix)

;; You can run the unit tests by calling

#+nil
(run! 'tests)

;; or

#+nil
(asdf:oos 'asdf:test-op 'lisp-matrix)


(def-suite tests
    :description "tests for the package LISP-MATRIX")

;;; Basic tests

(def-suite matrix-double :in tests
           :description "basic tests on double matrices")

(in-suite matrix-double)

(defun random-array (n m)
  "Return a random 2D array of size N x M."
  (make-array (list n m)
              :element-type 'double-float
              :initial-contents
              (loop for i below n collect
                    (loop for j below m collect
                          (random 1d0)))))

(defun test-matrix-size (matrix n m)
  "test all size functions of MATRIX against N and M"
  (is (= (nrows matrix) n))
  (is (= (ncols matrix) m))
  (is (= (nelts matrix) (* n m)))
  (is (= (matrix-dimension matrix 0) n))
  (is (= (matrix-dimension matrix 1) m))
  (signals error (matrix-dimension matrix 2))
  (signals error (matrix-dimension matrix -1))
  (is (equal (matrix-dimensions matrix)
	     (list n m))))

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

(test make-matrix-double-zero-size
  "make a matrix which has zero size"
  #-clisp
  (for-all-implementations
    (finishes (make-matrix 0 0))
    (finishes (make-matrix 0 1))
    (finishes (make-matrix 1 0)))
  #+clisp
  (for-implementations (:lisp-array)
    (finishes (make-matrix 0 0))
    (finishes (make-matrix 0 1))
    (finishes (make-matrix 1 0)))
  #+clisp
  (fiveam:skip "matrix with zero size don't work in CLISP"))

(test make-matrix-double-1
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

(test make-matrix-double-2
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

(test make-matrix-double-3
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

(test make-matrix-double-4
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

(test transpose-double
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

(test window-double
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

(test m=
  (for-all-implementations
    (for-all ((n (gen-integer :min 1 :max 10))
              (m (gen-integer :min 1 :max 10)))
      (let ((a (rand n m)))
        (is
         (m= (make-matrix n m :initial-contents a)
             (make-matrix n m :initial-contents a)))))
    (is (not (m= (make-matrix 1 2)
                 (make-matrix 1 1))))
    (is (not (m= (make-matrix 2 1)
                 (make-matrix 1 1))))
    (is (not (m= (make-matrix 1 1 :initial-element 1d0)
                 (make-matrix 1 1 :initial-element 0d0))))))

(test setf-mref
  (for-all-implementations
    (for-all ((n (gen-integer :min 0 :max 10) #+clisp (> n 0))
              (m (gen-integer :min 0 :max 10) #+clisp (> m 0)))
      (let ((a (make-matrix n m))
            (b (rand n m)))    
        (finishes
          (dotimes (i n)
            (dotimes (j m)
              (setf (mref a i j) (mref b i j)))))
        (is (m= a b))))))

(test transposed-p
  (for-all-implementations
    (let ((m (make-matrix 1 1)))
      (is (null (transposed-p m)))
      (is (transposed-p (transpose m)))
      (is (transposed-p (transpose (transpose m))))
      (is (transposed-p (window (transpose m)))))))

(test zero-offset-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (is (zero-offset-p m))
      (is (zero-offset-p (transpose m)))
      (is (zero-offset-p (transpose (transpose m))))
      (is (zero-offset-p (window m :nrows 1)))
      (is (zero-offset-p (strides m :ncols 1)))
      (is (not (zero-offset-p (window m :row-offset 1 :nrows 1))))
      (is (not (zero-offset-p (window m :col-offset 1 :ncols 1))))
      (is (not (zero-offset-p (strides m :row-offset 1 :nrows 1))))
      (is (not (zero-offset-p (strides m :col-offset 1 :ncols 1))))
      (is (not (zero-offset-p (window (strides m :col-offset 1 :ncols 1)))))
      (is (zero-offset-p (strides m :row-stride 2 :nrows 2))))))

(test unit-stride-p
  (for-all-implementations
    (let ((m (make-matrix 3 3)))
      (is (unit-stride-p m))
      (is (unit-stride-p (transpose m)))
      (is (unit-stride-p (transpose (transpose m))))
      (is (unit-stride-p (window m :nrows 1)))
      (is (unit-stride-p (strides m :ncols 1)))
      (is (unit-stride-p (window m :row-offset 1 :nrows 1)))
      (is (unit-stride-p (window m :col-offset 1 :ncols 1)))
      (is (unit-stride-p (strides m :row-offset 1 :nrows 1)))
      (is (unit-stride-p (strides m :col-offset 1 :ncols 1)))
      (is (not (unit-stride-p (strides m :row-stride 2 :nrows 2))))
      (is (not (unit-stride-p (transpose (strides m :row-stride 2 :nrows 2)))))
      (is (not (unit-stride-p (window (strides m :row-stride 2 :nrows 2)))))
      (is (not (unit-stride-p (strides (strides m :row-stride 2 :nrows 2))))))))

(test copy
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
                 (is (test-copy-m= a b))
                 (is (test-copy-m= b c))
                 (is (test-copy-m= a c)))))
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

(test rand
  (for-all-implementations
    (let* ((state1 (make-random-state))
           (state2 (make-random-state state1)))
      (is (m= (rand 2 3 :state state1)
              (rand 2 3 :state state2)))
      (is (not (m= (rand 2 3 :state state1)
                   (rand 2 3 :state state1)))))))

;;; Test lapack

(def-suite lapack :in tests
           :description "tests for lapack methods")

(in-suite lapack)

(test make-predicate
  (is (equal (make-predicate 'unit-stride-p)
             'unit-stride-p))
  (is (equal (make-predicate '(not unit-stride-p))
             '(lambda (a)
               (not (unit-stride-p a)))))
  (is (equal (make-predicate '(or (not unit-stride-p)
                               (not zero-offset-p)))
             '(lambda (a)
               (or (not (unit-stride-p a))
                (not (zero-offset-p a))))))
  (is (equal (make-predicate '(or (not unit-stride-p)
                               (not zero-offset-p)
                               transposed-p))
             '(lambda (a)
               (or (not (unit-stride-p a))
                (not (zero-offset-p a))
                (transposed-p a)))))
  (is (equal (make-predicate 't)
             '(constantly t)))
  (is (equal (make-predicate 'nil)
             '(constantly nil))))

(test datatypes
  (is (string= (datatype->letter 'float) "S"))
  (is (string= (datatype->letter 'double) "D"))
  (is (string= (datatype->letter 'complex-float) "C"))
  (is (string= (datatype->letter 'complex-double) "Z")))

(test scal
  (is
   (m=
    (scal 1.5d0 (ones 2 2 :element-type 'double-float))
    (make-matrix 2 2 :element-type 'double-float
                     :initial-element 1.5d0)))
  (is
   (m=
    (scal 1.5 (ones 2 2 :element-type 'single-float))
    (make-matrix 2 2 :element-type 'single-float
                     :initial-element 1.5)))
  (is
   (m=
    (scal #C(1.5 1.5)
          (ones 2 2 :element-type '(complex single-float)))
    (make-matrix 2 2 :element-type '(complex single-float)
                     :initial-element #C(1.5 1.5))))
  (is
   (m=
    (scal #C(1.5d0 1.5d0)
          (ones 2 2 :element-type '(complex double-float)))
    (make-matrix 2 2 :element-type '(complex double-float)
                     :initial-element #C(1.5d0 1.5d0)))))

(def-suite gemm :in lapack
           :description "tests of the M* function")

(in-suite gemm)

(defun check-m* (a b)
  (let ((result (make-matrix 2 2 :initial-contents
                             '((19d0 22d0)
                               (43d0 50d0)))))
    (is (m= result (m* a b)))))

(defmacro def-m*-test (name a b)
  `(test ,name
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

(test gemm-window-c-copy
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 3 3))
           (windowed-c (window c :nrows 2 :ncols 2)))
      (is (eq windowed-c
              (gemm 1d0
                    (make-matrix 2 2 :initial-contents
                                 '((1d0 2d0)
                                   (3d0 4d0)))
                    (make-matrix 2 2 :initial-contents
                                 '((5d0 6d0)
                                   (7d0 8d0)))
                    0d0
                    windowed-c)))
      (is (m= windowed-c result))
      (is (m= windowed-c (window c :nrows 2 :ncols 2)))
      (is (m= (window c :nrows 1 :row-offset 2)
              (zeros 1 3)))
      (is (m= (window c :ncols 1 :col-offset 2)
              (zeros 3 1))))))

(test gemm-window-c-copy-copyback
  (for-all-implementations
    (let* ((result (make-matrix 2 2 :initial-contents
                                '((19d0 22d0)
                                  (43d0 50d0))))
           (c (zeros 4 4))
           (windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
                                                   :col-offset 2)))
      (is (eq windowed-c
              (gemm 1d0
                    (make-matrix 2 2 :initial-contents
                                 '((1d0 2d0)
                                   (3d0 4d0)))
                    (make-matrix 2 2 :initial-contents
                                 '((5d0 6d0)
                                   (7d0 8d0)))
                    0d0
                    windowed-c)))
      (is (m= windowed-c result))
      (is (m= windowed-c (window c :nrows 2 :ncols 2 :row-offset 2
                                                     :col-offset 2)))
      (is (m= (window c :nrows 2) (zeros 2 4)))
      (is (m= (window c :ncols 2) (zeros 4 2))))))


(test m*-double
  (for-all-implementations
    (is
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

(test m*-single
  (for-all-implementations
    (is
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

(test m*-complex-single
  (for-all-implementations
    (is
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

(test m*-complex-double
  (for-all-implementations
    (is
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
                                           (#C(43d0 0d0) #C(50d0 0d0))))))))
