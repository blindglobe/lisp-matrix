;;;
;;; Time-stamp: <2008-05-03 21:10:12 Evan Monroig>

(in-package :lisp-matrix)

(def-suite multiplication :in tests
           :description "tests for matrix multiplication and lapack
           wrappers")

(in-suite multiplication)

;;; Just a basic try at multiplication using CL-BLAPACK.  This code
;;; was just to make sure that it actually worked, and you can see
;;; code duplication at its best !
;;;
;;; Next step is to integrate it all using things like +OUR-FUNCTIONS+
;;; and GENERATE-METHODS in experimental/matview.lisp (see below at
;;; the next block of comments)
;;;
;;;
;;;
;;; Quick look at the LAPACK functions that I (Evan Monroig) use:
;;; 
;;; %DGEMM -- can handle transposed matrices, and windowed matrices if
;;; OFFSET0 and OFFSET1 are zero => no need to copy the arguments
;;;
;;; %DGELSY -- overwrite A and B => may need to copy, esp. for B since
;;; we need it to be of correct size to hold the result matrix
;;;
;;; %DGESVD -- overwrite A; outputs depend on argument parameters =>
;;; need to be wrapped manually
;;;
;;;
;;; In general in LAPACK, we need to provide work or output matrices
;;; of correct sizes as specified in the documentation.  Therefore,
;;; doing the translation automatically seems to be a natural language
;;; processing task.  We don't want to do that.
;;;
;;; A better way is probably to do as for MATLISP, manually wrap the
;;; fortran function into lispy interfaces as people need them.

#+nil
(let ((a (make-matrix 2 2 'double
                      :initial-contents
                      (make-array '(2 2) :initial-contents
                                  '((1d0 1d0) (1d0 1d0)))))
      (b (make-matrix 2 2 'double
                      :initial-contents
                      (make-array '(2 2) :initial-contents
                                  '((1d0 0d0) (0d0 2d0)))))
      (res (make-matrix 2 2 'double)))
  (%dgemm "N" "N" 2 2 2 1d0 (data b) 2 (data a) 2 0d0 (data res) 2)
  res)

(defgeneric m* (a b))

(defmethod m* ((a matrix-double) (b matrix-double))
  (let ((m (nrows a))
        (n (ncols b))
        (k (ncols a)))
    (assert (= k (nrows b)))
    (let ((res (make-matrix n m 'double)))
      (%dgemm "N" "N" m n k 1d0 (data a) m (data b) k 0d0 (data res) n)
      res)))

(defmethod m* ((a transpose-matview-double) (b matrix-double))
  (let ((m (nrows a))
        (n (ncols b))
        (k (ncols a)))
    (assert (= k (nrows b)))
    (let ((res (make-matrix n m 'double)))
      (%dgemm (ecase (orientation a)
                (:column "N")
                (:row "T"))
              (ecase (orientation b)
                (:column "N")
                (:row "T"))
              m n k 1d0 (data a) m (data b) k 0d0 (data res) n)
      res)))

(defmethod m* ((a matrix-double) (b transpose-matview-double))
  (let ((m (nrows a))
        (n (ncols b))
        (k (ncols a)))
    (assert (= k (nrows b)))
    (let ((res (make-matrix n m 'double)))
      (%dgemm (ecase (orientation a)
                (:column "N")
                (:row "T"))
              (ecase (orientation b)
                (:column "N")
                (:row "T"))
              m n k 1d0 (data a) m (data b) k 0d0 (data res) n)
      res)))

(defmethod m* ((a transpose-matview-double) (b transpose-matview-double))
  (let ((m (nrows a))
        (n (ncols b))
        (k (ncols a)))
    (assert (= k (nrows b)))
    (let ((res (make-matrix n m 'double)))
      (%dgemm (ecase (orientation a)
                (:column "N")
                (:row "T"))
              (ecase (orientation b)
                (:column "N")
                (:row "T"))
              m n k 1d0 (data a) m (data b) k 0d0 (data res) n)
      res)))

(test m*
  ;; basic test
  (is
   (m= (m* (make-matrix 2 2 'double :initial-contents
                        '((1d0 2d0) (3d0 4d0)))
           (make-matrix 2 2 'double :initial-contents
                        '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose A
  (is
   (m= (m* (transpose
            (make-matrix 2 2 'double :initial-contents
                         '((1d0 3d0) (2d0 4d0))))
           (make-matrix 2 2 'double :initial-contents
                        '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose B
  (is
   (m= (m* (make-matrix 2 2 'double :initial-contents
                        '((1d0 2d0) (3d0 4d0)))
           (transpose
            (make-matrix 2 2 'double :initial-contents
                         '((5d0 7d0) (6d0 8d0)))))
       (make-matrix 2 2 'double :initial-contents
                        '((19d0 22d0) (43d0 50d0)))))
  ;; double transpose A
  (is
   (m= (m* (transpose
            (transpose
             (make-matrix 2 2 'double :initial-contents
                          '((1d0 2d0) (3d0 4d0)))))
           (make-matrix 2 2 'double :initial-contents
                        '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose A and B
  (is
   (m= (m* (transpose
            (make-matrix 2 2 'double :initial-contents
                         '((1d0 3d0) (2d0 4d0))))
           (transpose
            (make-matrix 2 2 'double :initial-contents
                         '((5d0 7d0) (6d0 8d0)))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0))))))



;;; Follows some steps to implement lapack functions simultaneously
;;; for all matrix types and while avoiding unnecessary matrix copies.
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

(defgeneric transposed-p (matrix)
  (:documentation "Is MATRIX a transposed view of its ancestor
  matrix?"))

(defmethod transposed-p ((matrix matrix-like))
  (and (parent matrix)
       (transposed-p (parent matrix))))

(defmethod transposed-p ((matrix transpose-matview))
  t)

(test transposed-p
  (let ((m (make-matrix 1 1 'double)))
   (is (null (transposed-p m)))
   (is (transposed-p (transpose m)))
   (is (transposed-p (transpose (transpose m))))
   (is (transposed-p (window (transpose m))))))

(defgeneric zero-offset-p (matrix)
  (:documentation "Has MATRIX a zero offset (as for window and stride
  views)?"))

(defmethod zero-offset-p ((matrix matrix-like))
  (if (parent matrix)
      (zero-offset-p (parent matrix))
      t))

(defmethod zero-offset-p ((matrix window-matview))
  (and (= 0 (offset0 matrix) (offset1 matrix))
       (zero-offset-p (parent matrix))))

(test zero-offset-p
  (let ((m (make-matrix 3 3 'double)))
   (is (zero-offset-p m))
   (is (zero-offset-p (transpose m)))
   (is (zero-offset-p (transpose (transpose m))))
   (is (zero-offset-p (window m :nrows 1)))
   (is (zero-offset-p (strides m :ncols 1)))
   (is (not (zero-offset-p (window m :offset0 1 :nrows 1))))
   (is (not (zero-offset-p (window m :offset1 1 :ncols 1))))
   (is (not (zero-offset-p (strides m :offset0 1 :nrows 1))))
   (is (not (zero-offset-p (strides m :offset1 1 :ncols 1))))
   (is (not (zero-offset-p (window (strides m :offset1 1 :ncols 1)))))
   (is (zero-offset-p (strides m :stride0 2 :nrows 2)))))

(test unit-stride-p
  (let ((m (make-matrix 3 3 'double)))
   (is (unit-stride-p m))
   (is (unit-stride-p (transpose m)))
   (is (unit-stride-p (transpose (transpose m))))
   (is (unit-stride-p (window m :nrows 1)))
   (is (unit-stride-p (strides m :ncols 1)))
   (is (unit-stride-p (window m :offset0 1 :nrows 1)))
   (is (unit-stride-p (window m :offset1 1 :ncols 1)))
   (is (unit-stride-p (strides m :offset0 1 :nrows 1)))
   (is (unit-stride-p (strides m :offset1 1 :ncols 1)))
   (is (not (unit-stride-p (strides m :stride0 2 :nrows 2))))
   (is (not (unit-stride-p (transpose (strides m :stride0 2 :nrows 2)))))
   (is (not (unit-stride-p (window (strides m :stride0 2 :nrows 2)))))
   (is (not (unit-stride-p (strides (strides m :stride0 2 :nrows 2)))))))

(defun has-offset-or-nonunit-stride-p (matrix)
  (not (and (zero-offset-p matrix)
            (unit-stride-p matrix))))


(defgeneric copy-into (a b)
  (:documentation "Copy A into B if they are not the same object, and
  return B.  A and B should have the same dimensions."))

(defmethod copy-into ((a matrix-like) (b matrix-like))
  (assert (= (ncols a) (ncols b)))
  (assert (= (nrows a) (nrows b)))
  ;; FIXME: care about fast copy once everything is working
  (unless (eq a b)
    (dotimes (i (nrows a) b)
      (dotimes (j (ncols a))
        (setf (mref b i j) (mref a i j)))))
  b)

(defgeneric copy (a)
  (:documentation "Return a deep copy of a matrix A."))

(defmethod copy ((a matrix-like))
  (copy-into a (make-matrix (nrows a) (ncols a) (fnv-type a))))

(test copy
  (labels ((test-copy-m= (a b)
             (and (not (eq a b))
                  (m= a b)))
           (test-copy (a)
             (let ((b (copy a))
                   (c (make-matrix (nrows a) (ncols a) (fnv-type a))))
               (finishes (copy-into a c))
               (is (test-copy-m= a b))
               (is (test-copy-m= b c))
               (is (test-copy-m= a c)))))
    (for-all ((n (gen-integer :min 0 :max 10))
              (m (gen-integer :min 0 :max 10))
              (n2 (gen-integer :min 0 :max 10) (<= n2 n))
              (m2 (gen-integer :min 0 :max 10) (<= m2 m))
              (row-offset (gen-integer :min 0 :max 10)
                          (<= row-offset (- n n2)))
              (col-offset (gen-integer :min 0 :max 10)
                          (<= col-offset (- m m2))))
      (test-copy (make-matrix n m 'double :initial-contents
                              (random-array n m)))
      (test-copy (transpose
                  (make-matrix n m 'double :initial-contents
                               (random-array n m))))
      (test-copy (window
                  (make-matrix n m 'double :initial-contents
                               (random-array n m))
                  :nrows n2 :ncols m2 :offset0 row-offset :offset1
                  col-offset)))))

(defgeneric copy-maybe (a test)
  (:documentation "Return a deep copy of a matrix A if TEST is
  satisfied, or return A itself."))

(defmethod copy-maybe ((a matrix-like) test)
  (if (funcall test a)
      (copy a)
      a))

(defun build-test-lambda (form)
  "From an expression combining predicates, construct a function of
one argument that evaluates the logical expression on the element,
where each predicate is applied to the argument to obtain its logical
value."
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

(test build-test-lambda
  (is (equal (build-test-lambda 'unit-stride-p)
             'unit-stride-p))
  (is (equal (build-test-lambda '(not unit-stride-p))
             '(lambda (a)
               (not (unit-stride-p a)))))
  (is (equal (build-test-lambda '(or (not unit-stride-p)
                                  (not zero-offset-p)))
             '(lambda (a)
               (or (not (unit-stride-p a))
                (not (zero-offset-p a))))))
  (is (equal (build-test-lambda '(or (not unit-stride-p)
                                  (not zero-offset-p)
                                  transposed-p))
             '(lambda (a)
               (or (not (unit-stride-p a))
                (not (zero-offset-p a))
                (transposed-p a)))))
  (is (equal (build-test-lambda 't)
             '(constantly t)))
  (is (equal (build-test-lambda 'nil)
             '(constantly nil))))

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
                                           ,(build-test-lambda predicate)))))
                         forms gensyms))
           ,@body
           ,@(loop for form in forms
                for g in gensyms
                when (third form)
                collect `(copy-into ,g ,(first form)))))
       ,result)))

#+nil
(with-copies ((a (or (not unit-stride-p)
                     (not zero-offset-p)))
              (b (or (not unit-stride-p)
                     (not zero-offset-p)))
              (c (or (not unit-stride-p)
                     (not zero-offset-p)
                     transposed-p)
                 t))
    c
  (%dgemm (orientation-letter a)
          (orientation-letter b)
          (nrows a)
          (ncols b)
          (ncols a)
          alpha
          (data a)
          (real-nrows a)
          (data b)
          (real-nrows b)
          beta
          (data c)
          (real-nrows c)))

;;; Datatypes that are supported by LAPACK and the BLAS.
(defparameter *supported-datatypes*
  '((single-float . "s")
    (double-float . "d")
    ((complex single-float) . "c")
    ((complex double-float) . "z"))
  "Association list mapping each supported datatype to its BLAS/LAPACK
letter.")

(defun datatype->letter (datatype)
  "Converts the given DATATYPE to the letter that symbolizes it in the
BLAS and LAPACK."
  (or (cdr (assoc datatype *supported-datatypes* :test #'equal))
      (error "LAPACK does not support the datatype ~A" datatype)))

(test datatypes
  (is (string= (datatype->letter 'single-float) "s"))
  (is (string= (datatype->letter 'double-float) "d"))
  (is (string= (datatype->letter '(complex single-float)) "c"))
  (is (string= (datatype->letter '(complex double-float)) "z")))

(defun orientation->letter (orientation)
  "Return the LAPACK letter corresponding to ORIENTATION."
  (ecase orientation
    (:column "N")
    (:row "T")))

(defun orientation-letter (a)
  "Return the LAPACK letter corresponding to the orientation of the
matrix A."
  (orientation->letter (orientation a)))

(defgeneric real-nrows (a)
  (:documentation "Return the number of rows of the ancestor of A."))

(defgeneric real-ncols (a)
  (:documentation "Return the number of columns of the ancestor of
  A."))

(defmethod real-nrows ((a matrix-like))
  (if (parent a)
      (real-nrows (parent a))
      (nrows a)))

(defmethod real-ncols ((a matrix-like))
  (if (parent a)
      (real-ncols (parent a))
      (nrows a)))

(defmethod gemm (alpha (a matrix-double-like) (b matrix-double-like)
                 &optional
                 (beta 0d0) c)
  (assert (= (ncols a) (nrows b)))
  (unless c
    (setq c (make-matrix (nrows a) (ncols b) 'double)))
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
    (%dgemm (orientation-letter a)
            (orientation-letter b)
            (nrows a)
            (ncols b)
            (ncols a)
            alpha
            (data a)
            (real-nrows a)
            (data b)
            (real-nrows b)
            beta
            (data c)
            (real-nrows c))))

#+nil
(def-lapack-method gemm (alpha a b &optional (beta 0d0) c)
  (assert (= (ncols a) (nrows b)))
  (unless c
    (setq c (make-matrix (nrows a) (ncols b) !datatype)))
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
    (!function (orientation-letter a)
               (orientation-letter b)
               (nrows a)
               (ncols b)
               (ncols a)
               alpha
               (data a)
               (real-nrows a)
               (data b)
               (real-nrows b)
               beta
               (data c)
               (real-nrows c))))

(test gemm
  ;; basic test
  (is
   (m= (gemm 1d0
             (make-matrix 2 2 'double :initial-contents
                          '((1d0 2d0) (3d0 4d0)))
             (make-matrix 2 2 'double :initial-contents
                          '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose A
  (is
   (m= (gemm 1d0
             (transpose
              (make-matrix 2 2 'double :initial-contents
                           '((1d0 3d0) (2d0 4d0))))
             (make-matrix 2 2 'double :initial-contents
                          '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose B
  (is
   (m= (gemm 1d0
             (make-matrix 2 2 'double :initial-contents
                          '((1d0 2d0) (3d0 4d0)))
             (transpose
              (make-matrix 2 2 'double :initial-contents
                           '((5d0 7d0) (6d0 8d0)))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; double transpose A
  (is
   (m= (gemm 1d0
             (transpose
              (transpose
               (make-matrix 2 2 'double :initial-contents
                            '((1d0 2d0) (3d0 4d0)))))
             (make-matrix 2 2 'double :initial-contents
                          '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; transpose A and B
  (is
   (m= (gemm 1d0
             (transpose
              (make-matrix 2 2 'double :initial-contents
                           '((1d0 3d0) (2d0 4d0))))
             (transpose
              (make-matrix 2 2 'double :initial-contents
                           '((5d0 7d0) (6d0 8d0)))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; window A
  #+nil
  (is
   (m= (gemm 1d0
             (window
              (make-matrix 3 3 'double :initial-contents
                           '((1d0 3d0 0d0)
                             (2d0 4d0 0d0)
                             (0d0 0d0 0d0)))
              :nrows 2 :ncols 2)
             (make-matrix 2 2 'double :initial-contents
                          '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  #+nil
  (is
   (m= (gemm 1d0
             (window
              (make-matrix 3 3 'double :initial-contents
                           '((0d0 1d0 3d0)
                             (0d0 2d0 4d0)
                             (0d0 0d0 0d0)))
              :nrows 2 :ncols 2 :offset1 1)
             (make-matrix 2 2 'double :initial-contents
                          '((5d0 6d0) (7d0 8d0))))
       (make-matrix 2 2 'double :initial-contents
                    '((19d0 22d0) (43d0 50d0)))))
  ;; stride A
  )
