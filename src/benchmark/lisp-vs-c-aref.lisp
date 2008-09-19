(in-package :cl-user)


(defpackage :matrix-benchmarks
  (:use :cl
	:cffi
	:lisp-matrix
	:matlisp)
  (:export :run-benchmark :run-benchmarks))


(in-package :matrix-benchmarks)

;;; Pull out all the optimization stops.
(declaim (optimize (safety 0) (debug 0) (speed 3)))

;;; Make sure you've loaded ASDF and your CFFI directory was pushed 
;;; onto asdf:*central-registry*.
(asdf:oos 'asdf:load-op :cffi)

;;; We set the array size to 4 million so that the benchmark takes a
;;; decent amount of time to run and so that the array doesn't fit in
;;; cache (MAKE-ARRAY with the :INITIAL-ELEMENT keyword causes the 
;;; array to be written to, meaning that the whole array could be in 
;;; the cache).  Of course if you have an IBM Power5 with its monster
;;; 36 MB L3 cache, you might want to change this value :P
(defconstant +ASIZE+ 4000000)
(declaim (type fixnum +ASIZE+))


(defmacro fill-foreign-array (array-name type count with-what)
  "Fills the given foreign array ARRAY-NAME with COUNT elements 
   WITH-WHAT, each of which are of (CFFI) type TYPE.  Macro-
   expansion fails if the macro doesn't recognize the given type."
  (cond ((or (eq type :double) (eq type 'double))
	 `(dotimes (i ,count)
	    (declare (type fixnum i ,count))
	    (setf (cffi:mem-aref ,array-name :double i) 
	      (the double-float ,with-what))))
	((or (eq type :int) (eq type 'int))
	 `(dotimes (i ,count)
	    (declare (type fixnum i ,count))
	    (setf (cffi:mem-aref ,array-name :int i) 
	      ,with-what)))
	(t (error "I don't know how to fill with type ~A" type))))

;;; Here is a more general FILL-FOREIGN-ARRAY.  Note that it doesn't 
;;; do any type checking on ARRAY-NAME.
#|
(defmacro fill-foreign-array (array-name type count with-what)
  `(dotimes (i ,count)
     (declare (type fixnum i ,count))
     (setf (cffi:mem-aref ,array-name ,type i) ,with-what)))
|#   

(defmacro with-foreign-alloc ((array-name
			       type count
			       &optional (init-elt 0 init-elt-supplied-p))
			      &body body)
  "Allocates a foreign array (on the heap) named ARRAY-NAME of 
   CFFI type TYPE containing COUNT elements.  If INIT-ELT is 
   supplied, all the elements of ARRAY-NAME are set to INIT-ELT.  
   Then BODY is executed in an UNWIND-PROTECT, and the foreign 
   array is deallocated."
  (if init-elt-supplied-p
      `(let ((,array-name (cffi:foreign-alloc ,type :count ,count)))
	 (fill-foreign-array ,array-name ,type ,count ,init-elt)
	 (unwind-protect
	     (progn
	       ,@body)
	   (cffi:foreign-free ,array-name)))
    `(let ((,array-name (cffi:foreign-alloc ,type :count ,count))
	   (unwind-protect
	       (progn
		 ,@body)
	     (cffi:foreign-free ,array-name))))))
       


(defun lisp-aref-benchmark ()
  "Benchmark for a 1-D Lisp array, with AREF."
  (let ((A (make-array +ASIZE+ 
		       :element-type 'double-float 
		       :initial-element 1.0d0))
	(s 0.0d0))
    (declare (type double-float s))
    (declare (type (simple-array double-float (*)) A))
    (dotimes (i +ASIZE+ s)
       (declare (type fixnum i))
      (incf s (aref A i)))))



(defun foreign-aref-benchmark ()
  "Benchmark for a 1-D C array, with CFFI:MEM-AREF.  The function does pointer
   arithmetic every time it is called."
  (with-foreign-alloc (A :double +ASIZE+ 1.0d0)
    (let ((s 0.0d0))
      (declare (type double-float s))
      (dotimes (i +ASIZE+ s)
	(declare (type fixnum i))
	(incf s (cffi:mem-aref A :double i))))))


(defun optimized-foreign-aref-benchmark ()
  "Benchmark for a 1-D C array, with CFFI:MEM-REF instead of CFFI:MEM-AREF.
   We do the pointer arithmetic by hand.  CFFI makes this awkward since a 
   pointer may be an object different than an (unsigned-byte 32); you have
   to call INC-POINTER instead of just adding 8 to it.  Thus, the word
   \"optimized\" in the name of this function should be taken with a grain
   of salt."
  (with-foreign-alloc (A :double +ASIZE+ 1.0d0)
    (let ((s 0.0d0))
      (declare (type double-float s))
      ;; We need to make sure that B is a deep copy of A.
      ;; We'll be playing with B's address but we need to
      ;; hold on to A so that WITH-FOREIGN-ALLOC will 
      ;; deallocate the array correctly.
      (let ((B (cffi:make-pointer (cffi:pointer-address A))))
	(dotimes (i +ASIZE+ s)
	  (declare (type fixnum i))
	  (incf s (cffi:mem-ref B :double))
	  ;; inc-pointer might cons (it returns a new pointer).
	  (setf B (cffi:inc-pointer B 8)))))))

;;; lisp-matrix benchmark

(defun lisp-matrix-vref-benchmark ()
  (let ((a (make-vector +asize+ 'lisp-matrix::double :initial-element 1d0))
        (s 0d0))
    (declare (type double-float s)
             (type vector-double a))
    (dotimes (i +ASIZE+ s)
      (declare (type fixnum i))
      (incf s (vref a i)))))

;;; matlisp benchmarks

(defun matlisp-mref-benchmark ()
  (let ((a (matlisp::ones +ASIZE+ 1))
        (s 0d0))
    (declare (type double-float s)
             (type matlisp::real-matrix a))
    (dotimes (i +ASIZE+ s)
      (declare (type fixnum i))
      (incf s (matlisp::mref a i)))))

(defun matlisp-store-aref-benchmark ()
  (let* ((a (matlisp:ones +ASIZE+ 1))
         (s 0d0)
         (st (matlisp::store a)))
    (declare (type double-float s)
             (type matlisp::real-matrix a)
             (type (simple-array double-float (*)) st))
    (dotimes (i +ASIZE+ s)
      (declare (type fixnum i))
      (incf s (aref st i)))))
