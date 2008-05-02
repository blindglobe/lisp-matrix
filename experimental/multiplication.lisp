
;;; Just a basic try at multiplication using CL-BLAPACK.  This code
;;; was just to make sure that it actually worked, and you can see
;;; code duplication at its best !
;;;
;;; Next step is to integrate it all using things like +OUR-FUNCTIONS+
;;; and GENERATE-METHODS in experimental/matview.lisp
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

(in-package :lisp-matrix)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op 'org.middleangle.cl-blapack))

(use-package 'org.middleangle.cl-blapack)

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


(defun m= (a b)
  (and (= (nrows a) (nrows b))
       (= (ncols a) (ncols b))
       (dotimes (i (nrows a) t)
         (dotimes (j (nrows b) t)
           (unless (= (mref a i j) (mref b i j))
             (return))))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 2d0) (1d0 2d0)))
    (m* (make-matrix 2 2 'double :initial-contents '((1d0 1d0) (1d0 1d0)))
        (make-matrix 2 2 'double :initial-contents '((1d0 2d0) (1d0 2d0)))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 1d0) (2d0 2d0)))
    (m* (make-matrix 2 2 'double :initial-contents '((1d0 2d0) (1d0 2d0)))
        (make-matrix 2 2 'double :initial-contents '((1d0 1d0) (1d0 1d0)))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 2d0) (3d0 4d0)))
    (m* (make-matrix 2 2 'double :initial-contents '((5d0 6d0) (7d0 8d0)))
        (make-matrix 2 2 'double :initial-contents
                     '((19d0 22d0) (43d0 50d0)))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 3d0) (2d0 4d0)))
    (m* (transpose
         (make-matrix 2 2 'double :initial-contents '((5d0 7d0) (6d0 8d0))))
        (make-matrix 2 2 'double :initial-contents
                     '((19d0 22d0) (43d0 50d0)))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 3d0) (2d0 4d0)))
    (m* (make-matrix 2 2 'double :initial-contents '((5d0 6d0) (7d0 8d0)))
        (transpose
         (make-matrix 2 2 'double :initial-contents
                      '((19d0 43d0) (22d0 50d0))))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 2d0) (3d0 4d0)))
    (m* (transpose
         (transpose
          (make-matrix 2 2 'double :initial-contents '((5d0 6d0) (7d0 8d0)))))
        (make-matrix 2 2 'double :initial-contents
                     '((19d0 22d0) (43d0 50d0)))))

(m= (make-matrix 2 2 'double :initial-contents '((1d0 3d0) (2d0 4d0)))
    (m* (transpose
         (make-matrix 2 2 'double :initial-contents
                      '((5d0 7d0) (6d0 8d0))))
        (transpose
         (make-matrix 2 2 'double :initial-contents
                      '((19d0 43d0) (22d0 50d0))))))
