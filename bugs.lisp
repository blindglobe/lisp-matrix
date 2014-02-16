`
(in-package :lisp-matrix-user)
  
(M* (ones 2 2 :implementation :foreign-array)
    (ones 2 2 :implementation :foreign-array))

(element-type (ones 2 2 :implementation :foreign-array))
;; problem: it calls GEMM with FA-, FA-, and LA-....
#|
There is no applicable method for the generic function
  #<STANDARD-GENERIC-FUNCTION GEMM (8)>
when called with arguments
  (1.0d0
   #<FA-SIMPLE-MATRIX-DOUBLE  2 x 2
 1.0d0 1.0d0
 1.0d0 1.0d0>
   #<FA-SIMPLE-MATRIX-DOUBLE  2 x 2
 1.0d0 1.0d0
 1.0d0 1.0d0>
   0.0d0
   #<LA-SIMPLE-MATRIX-DOUBLE  2 x 2
 0.0d0 0.0d0
 0.0d0 0.0d0>).
   [Condition of type SIMPLE-ERROR]
|#  


(M* (ones 2 2 :implementation :lisp-array)
    (ones 2 2 :implementation :lisp-array))
;; works
  
(M* (ones 2 2 :implementation :lisp-array)
    (ones 2 2 :implementation :foriegn-array))
#|
There is no applicable method for the generic function
  #<STANDARD-GENERIC-FUNCTION MAKE-MATRIX* (2)>
when called with arguments
  (2 2 :FORIEGN-ARRAY :ELEMENT-TYPE DOUBLE-FLOAT
   :INITIAL-ELEMENT 1.0d0).
   [Condition of type SIMPLE-ERROR]
|#
