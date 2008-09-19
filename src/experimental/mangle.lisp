;;;; mangle.lisp
;;;; Author: mfh
;;;; Date: 15 Oct 2006
;;;; Last modified: 19 Oct 2006
;;;;
;;;; Functions for name-mangling, for a template system similar to 
;;;; that of C++ (except that it uses CLOS).  The mangling system 
;;;; is independent of the template system except when MANGLED-NAME
;;;; needs to output an error message, at which point it searches 
;;;; the TEMPLATES package for a *TEMPLATE-TYPES* symbol.  But we 
;;;; don't need a (USE-PACKAGE "TEMPLATES") or even to have loaded
;;;; the templates implementation yet.
;;;;
;;;; The mangling system is also package-friendly:  different 
;;;; packages have different mangling systems that are separate and 
;;;; guaranteed not to clash.
;;;; 
;;;; There are a number of parameters for tuning the mangling 
;;;; system, which are documented DEFCONSTANTs in the code below.
;;;;

(defpackage :mangle
  (:use :common-lisp)
  (:export :mangled-p
	   :mangle
	   :mangled-name))

(in-package :mangle)

;;; Length of the random symbol used for name mangling.
(defconstant +random-symbol-length+ 10)

;;; Characters for the name mangling are picked randomly 
;;; from this set, which is stored as a string (character array).
(defconstant +char-table+ #.(concatenate 'string 
			      "0123456789abcdefghijklmnopqrst"
			      "uvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
;;; Length of the above character table.
(defconstant +char-table-len+ (length +char-table+))


(defun random-mangling-string ()
  "Produces a random string of a fixed length, for use in name mangling."
  (let ((s (make-string +random-symbol-length+)))
    (loop for i from 0 upto (1- +random-symbol-length+) do
	  (declare (type fixnum +random-symbol-length+ +char-table-len+ i))
	  (declare (type string s))
	  (setf (char s i) (char +char-table+ (random +char-table-len+)))
	finally return s)))



;;;;;; TODO: get the SERIES package.
;;;(let* (;; Length of the random symbol used for name mangling.
;;;       (+random-symbol-length+ 10)
;;;       ;; Characters are picked randomly from this set.
;;;       (+char-table+ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")       
;;;       (+char-table-len+ (length +char-table+)))
;;;  (defun random-mangling-string ()
;;;    "Produces a random string of a fixed length, for use in name mangling."
;;;    (collect 'string
;;;	     (map-fn 'char 
;;;		     #'(lambda (i) 
;;;			 (declare (ignorable i))
;;;			 (char +char-table+ (random +char-table-len+)))
;;;		     (scan-range :length +random-symbol-length+)))))

;;; type-list is (cons classname template-value-list) or just classname
;;; if there are no template values.
(defun produce-instantiated-symbol-name (manglage type-list)
  (if (atom type-list)
      (format nil "~A-~A" type-list manglage)
    (reduce #'(lambda (x y) (concatenate 'string x "-" y)) 
	    (append (mapcar #'symbol-name type-list)
		    (list manglage)))))


;;; Hash table keying from package name to hash table, which keys
;;; from mangled name to unmangled type.  For example:
;;;
;;; COMMON-LISP -> 
;;; ('matrix-double-float-123456 -> (list 'matrix 'double-float))
;;;
;;; It can use EQ as the test because the keys are symbols.  We 
;;; start with :SIZE 1 because there may only be one package for 
;;; which we need a mangled->unmangled mapping.
(defparameter *mangled->unmangled* (make-hash-table :test #'eq :size 1))

;;; Hash table keying from package name to hash table, which keys
;;; from unmangled type to mangled name.  For example:
;;;
;;; COMMON-LISP ->
;;; (matrix double-float) -> matrix-double-float-123456
(defparameter *unmangled->mangled* (make-hash-table :test #'eq :size 1))



(defun mangled-p (type-list &key (package *package*))
  "TYPE-LIST:  either (CONS CLASSNAME TEMPLATE-VALUE-LIST, or CLASSNAME.
     The full name of the template type instantiation.
   PACKAGE:  the package in which the given template type instantiation
     was possibly instantiated.
   
   Returns T iff the given template type instantiation has been 
   instantiated in the given package."
  (multiple-value-bind (h is-there)
      (gethash package *unmangled->mangled*)
    (if is-there
	(multiple-value-bind (mangled is-there)
	    (if (atomp type-list)
		(let ((classname type-list))
		  (gethash (list classname) h))
	      (gethash type-list h))
	  is-there)
      nil)))
  
(defun mangled-name (type-list &key (package *package*))
  "If (CLASSNAME TEMPLATE-VALUE-LIST) is an instantiated template type
   in PACKAGE, returns the corresponding mangled name.  Otherwise, reports
   an informative error."
  (multiple-value-bind (u->m is-there)
      (gethash package *unmangled->mangled*)
    (if is-there
	;; Package has instantiated generic types in it.
	(multiple-value-bind (mangled is-there)
	    (if (atom type-list)
		(gethash type-list u->m)
	      (gethash (cons classname template-value-list) u->m))
	  (if is-there
	      ;; The given type has been instantiated;
	      ;; return the mangled name.
	      mangled
	    ;; Mangling wasn't registered.  Look up more info
	    ;; to print a useful error message.  EVERYTHING below
	    ;; this point is for printing an error message.
	    (let ((p->gts (find-symbol "*template-types*" "templates")))
	      (if (null p->gts)
		  (error "BUG: failed to find template types lookup table!")
		(multiple-value-bind (gts is-there)
		    (gethash package gts)
		  (if is-there
		      (multiple-value-bind (gt is-there)
			  (gethash classname gt)
			(if is-there
			    (if (atom type-list)
				(error (concatenate 'string 
					 "Template type ~A exists in package ~A, "
					 "but has not yet been instantiated")
					 type-list package)
			      (error (concatenate 'string
				       "Template type ~A exists in package ~A, "
				       "but has not yet been instantiated with "
				       "parameters ~A") (car type-list)
				       package (cdr type-list)))
			  (error "Template type ~A does not exist in package ~A"
				 (if (atom type-list) type-list (car type-list))
				 package)))
		    (error "No template types exist yet in package ~A" package)))))))
      (error "Package ~A contains no instantiated template types yet" package))))


(defun register-mangling-helper (m->u u->m type-list &key (package *package*))
  "M->U: hash table (for the given PACKAGE) from mangled name to unmangled type.
   U->M: hash table (for the given PACKAGE) from unmangled type to mangled name.
   CLASSNAME: template type in package PACKAGE.
   PACKAGE: package in which to intern the mangled name.  Defaults to the current
     package."
  
  (let ((type (if (atom type-list) (cons type-list nil) type-list))
	(symb (intern mangled package)))
    (progn
      (setf (gethash symb m->u) type)
      (setf (gethash type u->m) symb)
      symb)))

(defun register-mangling (mangled type-list &key (package *package*))
  "Registers the mangled name MANGLED for the template type instantiation
   (CLASSNAME TEMPLATE-VALUE-LIST) in package PACKAGE."
  ;; This is more complicated than necessary so that we only
  ;; call GETHASH once for each table, if the hash tables m->u 
  ;; and u->m are already set.
  (let ((m->u (gethash package *mangled->unmangled*))
	(u->m (gethash package *unmangled->mangled*)))
    (cond ((null m->u)
	   (setf (gethash package *mangled->unmangled)
	     (make-hash-table :test #'eq))
	   (cond ((null u->m)
		  (setf (gethash package *unmangled->mangled*)
		    (make-hash-table :test #'equal))
		  (register-mangling-helper (gethash package *mangled->unmangled*) 
					    (gethash package *unmangled->mangled*) 
					    type-list :package package))
		 (t 
		  (register-mangling-helper (gethash package *mangled->unmangled*) 
					    u->m type-list :package package))))
	  (t
	   (cond ((null u->m)
		  (setf (gethash package *unmangled->mangled*)
		    (make-hash-table :test #'equal))
		  (register-mangling-helper m->u
					    (gethash package *unmangled->mangled*) 
					    type-list :package package))
		 (t 
		  (register-mangling-helper m->u u->m type-list
					    :package package)))))))

;;; The MANGLE function generates mangled strings and tests to see 
;;; if they are in the mangling database; if they are, it tries again.
;;; This is the total number of times that it tries before it signals
;;; an error.  We insist on making this number non-infinite, in case
;;; +random-symbol-length+ is set too low or the random number 
;;; generator is broken; we don't want the mangling system to cause
;;; an infinite loop!
(defconstant +mangle-iteration-limit+ 10000)

(defun mangle (type-list &key (package *package*))
  "TYPE-LIST:  either an atom CLASSNAME or a list (CONS CLASSNAME 
     TEMPLATE-PARAMETER-LIST).  The name of a template type 
     instantiation, in which CLASSNAME is the template type.
   PACKAGE:  the name of the package in which the template type
     instantiation is to be instantiated.

   Given a template type instantiation, finds and registers a 
   corresponding mangled name, interning it in PACKAGE, and returns
   the mangled name.  If the mangling has already been registered,
   just returns the mangled name."

  (if (mangled-p type-list :package package)
      ;; If we've already registered a mangled name, just return it.
      (mangled-name type-list :package package)
    (loop
	for count from 0 to +mangle-iteration-limit+
	with symb = (produce-instantiated-symbol-name type-list
						      (random-mangling-string))
	while (find-symbol symb package)
	do 
	  (declare (type fixnum count +mangle-iteration-limit+))
	  (setf symb (produce-instantiated-symbol-name type-list
						       (random-mangling-string)))
	finally returning 
		(if (find-symbol symb package)
		    (error (concatenate 'string 
			     "Failed to find unique symbol for name "
			     "mangling of ~A after ~A iterations")
			   type-list +mangle-iteration-limit+)
		  (register-mangling symb type-list :package package)))))
