(in-package :lisp-matrix)

;; You can run the unit tests by calling

#+nil
(run! 'tests)

;; or

#+nil
(asdf:oos 'asdf:test-op 'lisp-matrix)


(def-suite tests
    :description "tests for the package LISP-MATRIX")

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

(test make-matrix-double-1 "default initial value"
  (for-all ((n (gen-integer :min 0 :max 100))
	    (m (gen-integer :min 0 :max 100)))
    (let (matrix)
     (finishes (setq matrix (make-matrix n m 'double)))
     (test-matrix-size matrix n m)
     (dotimes (i n)
       (dotimes (j m)
	 (unless (= (mref matrix i j) 0d0)
	   (fail "(mref matrix ~d ~d) is ~a, should be ~a"
		 i j (mref matrix i j) 0d0)))))))

(test make-matrix-double-2 "initial value to 1d0"
  (for-all ((n (gen-integer :min 0 :max 100))
	    (m (gen-integer :min 0 :max 100)))
    (let (matrix)
     (finishes (setq matrix (make-matrix n m 'double :initial-element
					 1d0)))
     (test-matrix-size matrix n m)
     (dotimes (i n)
       (dotimes (j m)
	 (unless (= (mref matrix i j) 1d0)
	   (fail "(mref matrix ~d ~d) is ~a, should be ~a"
		 i j (mref matrix i j) 1d0)))))))

(test make-matrix-double-3 "set initial contents"
  (for-all ((n (gen-integer :min 0 :max 100))
	    (m (gen-integer :min 0 :max 100)))
    (let ((array (random-array n m))	  
	  matrix)
     (finishes (setq matrix (make-matrix n m 'double
					 :initial-contents array)))
     (test-matrix-size matrix n m)
     (dotimes (i n)
       (dotimes (j m)
	 (unless (= (mref matrix i j) (aref array i j))
	   (fail "(mref matrix ~d ~d) is ~a, should be ~a"
		 i j (mref matrix i j) (aref array i j))))))))

(test transpose-double  
  (for-all ((n (gen-integer :min 0 :max 100))
	    (m (gen-integer :min 0 :max 100)))
    (let ((matrix1 (make-matrix n m 'double :initial-contents
                                (random-array n m)))
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
		  i j (mref matrix3 i j) (mref matrix1 i j))))))))

(test window-double
  (for-all ((n (gen-integer :min 0 :max 100))
            (m (gen-integer :min 0 :max 100))
            (n2 (gen-integer :min 0 :max 100) (<= n2 n))
            (m2 (gen-integer :min 0 :max 100) (<= m2 m))
            (row-offset (gen-integer :min 0 :max 100) (<= row-offset (- n n2)))
            (col-offset (gen-integer :min 0 :max 100) (<= col-offset (- m m2))))
    (let ((matrix1 (make-matrix n m 'double :initial-contents
                                (random-array n m)))
          matrix2)
      (finishes (setq matrix2 (window matrix1 :nrows n2 :ncols m2
                                      :offset0 row-offset
                                      :offset1 col-offset)))
      (test-matrix-size matrix2 n2 m2)
      (dotimes (i n2)
        (dotimes (j m2)
          (unless (= (mref matrix1 (+ i row-offset) (+ j col-offset))
                     (mref matrix2 i j))
            (fail "(mref matrix2 ~d ~d) is ~a, should be ~a"
                  i j (mref matrix1 (+ i row-offset) (+ j col-offset))
                  (mref matrix2 i j))))))))
