;;;; array-backend.lisp
;;;; Author: mfh
;;;; Created: 15 Oct 2006
;;;; Last modified: 21 Oct 2006
;;;;
;;;; Macros that hide how vectors and matrices are stored in memory.
;;;; Needs: CFFI features.lisp types.lisp
;;;;

(defpackage :array-backend
  (:use :common-lisp :types)
  (:export :fill-foreign-array
	   :ll-aref
	   :ll-make-array
	   :ll-displaced-array))
(in-package :array-backend)


;;; FIXME: Type declaration might be too general in the case that
;;; TYPE is not a constant at macro-expansion time.  If it is a 
;;; constant, we get a nice declaration.  NOTE:  LISP-ARRAY MUST
;;; be a SIMPLE-ARRAY, otherwise you're in trouble!!!
;;;
(defmacro copy-into-foreign (foreign-array lisp-array type count)
  (let ((index (gensym))
	(type-decl (if (constantp type)
		       `(declare (type ,lisp-array (simple-array ,(cffitype->lisp type) (*))))
		       `(declare (type ,lisp-array (simple-array * (*))))))
	(the-type (gensym)))
    (if (constantp type)
	`(dotimes (,index ,count ,foreign-array)
	   ,type-decl
	   (setf (cffi:mem-aref ,foreign-array ,(lisptype->cffi type) ,index)
		 (aref ,lisp-array ,index)))
	`(let ((,the-type (cffitype->lisp ,type)))
	   (dotimes (,index ,count ,foreign-array))
	   ,type-decl
	   (setf (cffi:mem-aref ,foreign-array ,the-type ,index)
		 (aref ,lisp-array ,index))))))

(defmacro copy-into-lisp (lisp-array foreign-array type count)
  (let ((index (gensym))
	(type-decl (if (constantp type)
		       `(declare (type ,lisp-array (simple-array ,(cffitype->lisp type) (*))))
		       `(declare (type ,lisp-array (simple-array * (*))))))
	(the-type (gensym)))
    (if (constantp type)
	`(dotimes (,index ,count ,lisp-array)
	   ,type-decl
	   (setf (aref ,lisp-array ,index)
		 (cffi:mem-aref ,foreign-array ,(lisptype->cffi type) ,index)))
	`(let ((,the-type (cffitype->lisp ,type)))
	   (dotimes (,index ,count ,lisp-array))
	   ,type-decl
	   (setf (aref ,lisp-array ,index) 
		 (cffi:mem-aref ,foreign-array ,the-type ,index))))))

	
		 

(defmacro fill-foreign-array (array-name type count with-what)
  "Fills the given foreign array ARRAY-NAME with COUNT elements
   WITH-WHAT."
  (let ((fill-with (gensym))
	(index (gensym))
	(cffi-type-expr (if (constantp type)
			    (lisptype->cffi type)
			  `(lisptype->cffi ,type)))
	(lisp-type-expr (if (constantp type)
			    (cffitype->lisp type)
			  `(cffitype->lisp ,type)))
	(coerce-expr (if (and (constantp type) (constantp with-what))
			 (coerce with-what (cffitype->lisp type))
			 `(coerce ,with-what (cffitype->lisp ,type)))))
    `(let ((,fill-with ,coerce-expr))
       (dotimes (,index ,count ,array-name)
	 (declare (type fixnum ,index ,count))
	 (setf (cffi:mem-aref ,array-name ,cffi-type-expr ,index) ,fill-with)))))

(defmacro ll-aref (array-name type index)
  "AREF for blocks of memory (see the GSL documentation for a 
   good definition of \"block\"), however those blocks may be 
   implemented.  You have to give the element type.  \"ll\"
   stands for \"low-level\"."
  (let ((cffi-type-expr (if (constantp type) (lisptype->cffi type)
			  `(lisptype->cffi ,type)))
	(lisp-type-expr (if (constantp type) (cffitype->lisp type)
			  `(cffitype->lisp ,type))))
    (if (has-feature-p :use-lisp-arrays)
	`(aref ,array-name ,index)
      ;; Use CFFI and foreign arrays.
      `(cffi:mem-aref ,array-name ,cffi-type-expr ,index))))

(defmacro ll-make-array (size type &key (initial-element 0 initial-element-supplied-p))
  "Call this function to allocate a block of memory to hold SIZE
   elements of type TYPE.  The :INITIAL-ELEMENT keyword, if a value
   is supplied with it, causes all elements of the block to be set
   to the given value (which is automatically coerced to the given
   type."
  (let* ((type-expr (if (constantp type)
		       (cffitype->lisp type)
		     `(cffitype->lisp ,type)))
	 (initial-element-expr
	  (if initial-element-supplied-p
	      (if (and (constantp initial-element)
		       (constantp type))
		  `(:initial-element ,(coerce initial-element type-expr))
		  `(:initial-element (coerce ,initial-element ,type-expr)))
	    nil))
	 (static-alloc-expr
	  (if (has-feature-p :static-arrays)
	      #+allegro '(:allocation :static-reclaimable)
	      #+gcl '(:static t)
	      #+lispworks '(:allocation :static)
	      #-(or allegro gcl lispworks)
	      (error "You claim that your Lisp supports static (pinned) allocation of arrays, but I don't know how to allocate static arrays in your Lisp.  Please fill in the appropriate code here.")
	      ;; no static arrays
	      nil)))
    (if (has-feature-p :use-lisp-arrays)
	`(make-array ,size 
		     :element-type ,type-expr 
		     ,@initial-element-expr
		     ,@static-alloc-expr)
      ;; Use C arrays.
      (if initial-element-supplied-p
	  `(fill-foreign-array (cffi:foreign-alloc ,type-expr :count ,size)
			       ,initial-element-expr)
	`(cffi:foreign-alloc ,type-expr :count ,size)))))


;;; #'cffi:foreign-free is the array finalization.
;;; Don't wrap it because that would introduce function call overhead.


(defmacro ll-displaced-array (old-array type offset)
  "Returns the displaced 1-D array to OLD-ARRAY (of type TYPE)
   at index OFFSET.  NOTE:  for C pointers, you should never 
   call free on the result of this macro call, because that 
   memory is owned by another object."
  (let ((cffi-type-expr (if (constantp type)
			    (lisptype->cffi type)
			  `(lisptype->cffi ,type)))
	(cffi-type-size-expr
	 (if (constantp type)
	     (cffi:foreign-type-size (lisptype->cffi type))
	   `(cffi:foreign-type-size (lisptype->cffi ,type))))
	(lisp-type-expr (if (constantp type)
			    (quote (cffitype->lisp type))
			  `(cffitype->lisp ,type)))
	(static-alloc-expr
	 (if (has-feature-p :static-arrays)
	     #+allegro '(:allocation :static-reclaimable)
	     #+gcl '(:static t)
	     #+lispworks '(:allocation :static)
	     #-(or allegro gcl lispworks)
	     (error "Don't know how to make static arrays in your lisp"))
	 nil))
    (if (has-feature-p :use-lisp-arrays)
	`(make-array (- (array-dimension ,old-array 0) ,offset)
		     ,@static-alloc-expr
		     :element-type ,lisp-type-expr
		     :displaced-to ,old-array
		     :displaced-index-offset ,offset)
      ;; Use C arrays.
      `(cffi:inc-pointer ,old-array (* ,offset ,cffi-type-size-expr)))))

