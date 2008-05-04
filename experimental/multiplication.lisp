;;;
;;; Time-stamp: <2008-05-04 13:13:57 Evan Monroig>

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

#+nil
(defmethod gemm (alpha (a matrix-double-like) (b matrix-double-like)
                 &optional
                 (beta 0d0) c)
  (assert (= (ncols a) (nrows b)))
  (unless c
    (setq c (make-matrix (nrows a) (ncols b) 'double)))
  (assert (= (nrows a) (nrows c)))
  (assert (= (ncols b) (ncols c)))
  (with-copies
      ((a (or (not unit-stride-p) (not zero-offset-p)))
       (b (or (not unit-stride-p) (not zero-offset-p)))
       (c (or (not unit-stride-p) (not zero-offset-p) transposed-p)
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
