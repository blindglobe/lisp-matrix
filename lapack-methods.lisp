(in-package :lisp-matrix)

;;; This file contains actual LAPACK methods.  See functions in
;;; lapack-utils.lisp for how supporting utility macros and functions.
;;;
;;; Time-stamp: <2008-06-06 16:40:12 Evan Monroig>

(def-lapack-method gemm (alpha (a !matrix-type) (b !matrix-type)
                               &optional (beta 0d0) c)
  (assert (= (ncols a) (nrows b)))
  (if c
      (progn
        (check-type c !matrix-type)
        (assert (= (nrows a) (nrows c)))
        (assert (= (ncols b) (ncols c))))
      (setq c (make-matrix (nrows a) (ncols b)
                           :element-type '!element-type)))
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
               a
               (real-nrows a)
               b
               (real-nrows b)
               beta
               c
               (real-nrows c))))

(defmacro call-with-work ((lwork work type) call)
  (let ((element-type (fnv-type->element-type type)))
    `(let ((work (make-matrix 1 1 :element-type ',element-type
                              :implementation :foreign-array))
           (,lwork -1))
       ,call
       (setq ,lwork (floor (mref ,work 0 0)))
       (setq ,work (make-vector ,lwork ',type))
       ,call)))

(defun check-info (info function-name)
  (unless (= info 0)
    (error "~a: error in argument ~d" function-name (- info))))

(def-lapack-method gelsy ((a !matrix-type) (b !matrix-type)
                          rcond &optional jpvt)
  ;; FIXME: has both LWORK and RWORK for %ZGELSY and %CGELSY
  (unless jpvt
    (setq jpvt (make-fnv-int32 (ncols a) :initial-value 0)))
  (let ((rank (make-fnv-int32 1 :initial-value 0))
        (info (make-fnv-int32 1 :initial-value 0)))
    ;; FIXME: B needs to be resized anyway if A has more columns than
    ;; rows, to allow for enough storage for the result matrix =>
    ;; quick fix: disallow this
    (assert (<= (ncols a) (nrows a)))
    (with-copies ((a (or (not unit-stride-p)
                         (not zero-offset-p)
                         transposed-p))
                  (b (or (not unit-stride-p)
                         (not zero-offset-p)
                         transposed-p)))
        ;; FIXME: the value RANK is not correct because the cffi type
        ;; :FORTRAN-INT does not define a TRANSLATE-FROM-FOREIGN method?
        ;; => why not use a standard cffi integer type anyway??
        ;; (a fix is to make RANK a fnv-int32 with one element
        (progn
          (check-info (fnv-int32-ref info 0) "GELSY")
          (list (if (= (nrows b) (ncols a))
                    b
                    (window b :nrows (ncols a)))
                (fnv-int32-ref rank 0)))
     (call-with-work (lwork work !data-type)
                     (!function (nrows a)
                                (ncols a)
                                (ncols b)
                                a
                                (real-nrows a)
                                b
                                (real-nrows b)
                                jpvt
                                rcond
                                rank
                                (data work)
                                lwork
                                info)))))

#+nil
(let ((*default-implementation* :lisp-array))
  (let* ((m 10)
         (n 10)
         (a (rand m n))
         (x (rand n 1))
         (b (gemm 1d0 a x))
         (rcond (* (coerce (expt 2 -52) 'double-float)
                   (max (nrows a) (ncols a))))
         (orig-a (copy a))
         (orig-b (copy b))
         (orig-x (copy x)))
    (list x (gelsy a b rcond))))
