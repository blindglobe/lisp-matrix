(in-package :lisp-matrix)

(defmacro with-typed-values ((&rest bindings) matrix &body body)
  "Each binding in BINDINGS is of the form (VARIABLE VALUE).  VARIABLE
  is bound to VALUE coerced to the element type of MATRIX."
  (with-unique-names (element-type)
    (labels ((make-coerced-binding (binding)
               (destructuring-bind (variable value) binding
                 `(,variable (coerce ,value ,element-type)))))
     `(let ((,element-type (element-type ,matrix)))
        (let (,@(mapcar #'make-coerced-binding bindings))
          ,@body)))))

(defgeneric m* (a b)
  (:documentation "Matrix multiplication: A * B.")
  (:method ((a matrix-like) (b matrix-like))
    (with-typed-values ((one 1)
                        (zero 0)) a
      (let ((c (make-matrix (nrows a) (ncols b)
                            :element-type (element-type a))))
        (gemm one a b zero c)))))

(defgeneric m+ (a b)
  (:documentation "Matrix addition: A + B.")
  (:method ((a matrix-like) (b matrix-like))
    (with-typed-values ((one 1)) a
      (axpy one a (copy b)))))

(defgeneric m- (a b)
  (:documentation "Matrix subtraction: A - B.")
  (:method ((a matrix-like) (b matrix-like))
    (with-typed-values ((minus-one -1)) a
      (axpy minus-one b (copy a)))))

;; TODO: SUM is not yet done
#+ (or)
(defgeneric sum (matrix)
  (:documentation "")
  (:method ((matrix matrix-like))
    (asum matrix)))
