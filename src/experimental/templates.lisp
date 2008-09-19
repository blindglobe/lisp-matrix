;;;; templates.lisp
;;;; Author: mfh
;;;; Created: 15 Oct 2006
;;;; Last modified: 19 Oct 2006
;;;;
;;;; A templated defclass mechanism somewhat like templates in C++.
;;;; Needs: mangle.lisp
;;;;
;;;; BUGS: Doesn't play nice with the class system yet.  There are 
;;;; some tricky problems to solve with the macrology.  I think the
;;;; DEFMETHOD INSTANTIATE created by the macro needs to be a closure
;;;; with access to the original package variable from which the 
;;;; macro was called.  It could be a problem if the macro is called
;;;; in a different package from which the INSTANTIATE method is 
;;;; called.
;;;;
;;;; UNTESTED!
;;;;

(defpackage :templates
  (:use :common-lisp :mangle)
  (:export :register-template-type
	   :template-type-p
	   :defclass-template))
(in-package :templates)

(defparameter *warnings* t)


;;; Hash table from package name, to set (implemented as a hash table)
;;; of generic classes in that package.
(defparameter *template-types* (make-hash-table :test #'eq :size 1))

(defun register-template-type (type &key (package *package*))
  "TYPE: either an atom CLASSNAME, or a list (CONS CLASSNAME TEMPLATE-PARAMETER-LIST).
   PACKAGE: a package in which to register TYPE as a template type.
 
   Registers TYPE as a template type in package PACKAGE.  If TYPE is an atom,
   it is registered as a template type with no template parameters; if TYPE
   is a list, it is registered as a template type named (CAR TYPE) with template
   parameters (CDR TYPE)."
  (let ((classname (if (atom type) type (car type)))
	(tpl (if (atom type) nil (cdr type))))
    (multiple-value-bind (h is-there)
	(gethash package *template-types*)
      (when (not is-there)
	(setf h (make-hash-table :test #'eq))
	(setf (gethash package *template-types*) h))
      (multiple-value-bind (old-tpl is-there)
	  (gethash classname h)
	(when (and is-there *warnings*)
	  (warn 
	   (concatenate 'string "Template class ~A has already been registered "
			"in package ~A with template parameter list ~A; now re-"
			"registering with template parameter list ~A") classname 
			package old-tpl tpl))
	(setf (gethash classname h) tpl)
	classname))))


(defun template-type-p (cl &key (package *package*))
  "Returns T iff the given template type CL is a template class 
   in package PACKAGE.  CL is a symbol, not a list."
  (multiple-value-bind (h is-there)
      (gethash package *template-types*)
    (and is-there
	 (multiple-value-bind (tpl is-there)
	     (gethash cl h)
	   is-there))))


(defun template-parameters (cl &key (package *package*))
  "Returns the list of template parameters associated with the 
   given template type CL."
  (multiple-value-bind (h is-there)
      (gethash package *template-types*)
    (if is-there
	(multiple-value-bind (tpl is-there)
	    (gethash cl h)
	  (if is-there
	      tpl
	    (error "Template type ~A not registered in package ~A" cl package)))
      (error "Package ~A has no template types registered in it" package))))
	  

(defun find-mangled-name (type &key (package *package*))
  "Returns the mangled name of the instantiation of TYPE
   in package PACKAGE."
  (if (template-type-p (if (atom type) type (car type)) package)
      (mangle:mangled-name type :package package)
    (error "Template type ~A is not registered as a template type in package ~A"
	   (if (atom type) type (car type)) package)))


(defun apply-macro (symb &rest args)
  (eval (cons symb args)))


(defun two-lists-to-alist (L1 L2)
  "Returns the ALIST resulting from looping over both input lists
   and consing an element from the first list onto the corresponding 
   element from the second list."
  (loop 
      for x in L1  
      for y in L2
      collect (cons x y)))

(defmacro defclass-template (classname template-param-list super-list 
			    &body body)
  "Create a class template like this:

   (defclass-template foo (bar baz) (s) BODY)

   Now you can instantiate your class, setting bar to double-float 
   and baz to 10, like this:

   (instantiate 'foo 'double-float 10)

   and the resulting class can be referred to as a 
   '(foo double-float 10).  (The actual type name is mangled,
   just like in C++, to prevent name collisions.  The type
   name is sufficiently obscure that you probably won't choose
   it for one of your classes.)  Note that unlike with C++, you
   have to instantiate your classes explicitly.
   
   Just like C++, there's a lot of code bloat -- a DEFCLASS 
   for each datatype for which you instantiate a class.  Code
   expands at macro-expansion time.

   You can also inherit from generic types, as long as those 
   types have been instantiate with the type with which you 
   want to instantiate this class.  You can inherit from non-
   generic types too -- the macro can tell the difference by
   checking if the superclass is in the set of \"generic\"
   (not CLOS sense, but template sense) classes.

   The instantiator macro is interned in the package in which 
   DEFCLASS-TEMPLATE is called, but the DEFCLASS and DEFTYPE
   are interned into the package in which the instantiator 
   macro is called.

   BUGS: you can't remove generic classes and instantiated 
   classes once you create them."
  (let ((class (gensym))
	(package *package*))
    (progn
      ;; Add the generic class to the set of generic classes.
      (register-template-type (cons classname template-param-list) :package package)
      
      `(progn 
	 (defmacro ,(intern (concatenate 'string "instantiate-" classname)) 
	     (&rest template-value-list)
	   
	   ;; Make sure that we've registered the generic class.
	   (assert (template-type-p ,classname))
	   
	   ;; Make sure that the number of template values to fill
	   ;; in is the same as the number of template parameters
	   ;; supplied to the generic class.
	   (assert (= (length (template-parameters ,classname :package package))
		      (length template-value-list)))
	   
	   (with-gensyms (mangled the-super-list the-body)
	     (let ((,mangled (mangle ,classname ,template-value-list))
		   (,the-super-list (mapcar #'(lambda (cl)
						(find-mangled-name
						 (cons cl template-value-list)))))
		   (,the-body (sublis (two-list-to-alist ,template-param-list
							 template-value-list))))
	       `(progn
		  (defclass ,mangled ,the-super-list ,the-body)
		  (deftype ,classname ,template-param-list
		    `,(mangled-name ,classname ,template-param-list))))))
	 
	 ;; Create the INSTANTIATE method, EQL-specialized to the
	 ;; generic class name.  It just calls the associated macro.
	 ;; We use the EVAL construct because we have to compute the
	 ;; name of the instantiate-<classname> macro, and the only
	 ;; way to call a macro whose name is computed is to call EVAL
	 ;; (APPLY or FUNCALL don't work for macros).
	 `(defmethod instantiate ((,class (eql ,classname))
				  &rest tvl)
	    (templates::apply-macro ,(find-symbol (format nil 
							  "instantiate-~A" 
							  ,classname 
							  ,package))
				    ,@tvl))))))

