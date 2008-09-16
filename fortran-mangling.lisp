;;;; Author: mfh
;;;; Date: 30 Dec 2006
;;;; Last modified: 31 Dec 2006
;;;; 
;;;; Package for figuring out the Fortran name mangling scheme used by
;;;; your linker for BLAS and LAPACK functions.
;;;;
;;;; GNU Autoconf has macros (e.g. AC_FC_FUNC) for determining how a
;;;; Fortran function name is mangled by the Fortran compiler.  It's
;;;; interesting to note that Fortran names with underscores may be
;;;; mangled differently by some compilers than Fortran names without
;;;; underscores.
;;;;
;;;; We have chosen, however, not to invoke Autoconf to figure out the
;;;; mangling scheme, as it would create a dependency on the whole
;;;; Autoconf chain and the shell necessary for invoking it.  Instead, 
;;;; we observe that as of LAPACK 3.0, the BLAS and LAPACK Fortran 
;;;; function names do not have underscores in them, so we can rely on
;;;; simple ad hoc tests based on known Fortran compilers.  
;;;;
;;;; The SuperLU 3.0 source code, which was designed for maximum
;;;; compatibility on IBM AIX, Sun Solaris and GNU/Linux systems,
;;;; supports three different Fortran name mangling schemes: 
;;;;
;;;; 1. No change (linker uses same name as in the Fortran source)
;;;; 2. Append one underscore (_)
;;;; 3. Change from lowercase to all uppercase
;;;;
;;;; If we test all combinations of these schemes as well as some
;;;; obvious similar schemes (e.g. prepend underscore, append two
;;;; underscores, change from uppercase to lowercase), we should cover
;;;; most reasonable Fortran name mangling schemes.  Note that Fortran
;;;; 77 name mangling is much less annoying than C++ name mangling,
;;;; due to the lack of classes and namespaces.  Fortran >= 90 name
;;;; mangling may be another story, but the BLAS and LAPACK (as of
;;;; LAPACK 3.0) have chosen not to use the "object-oriented" features
;;;; of Fortran >= 90 that may complicate name mangling.  As we are
;;;; only interested in name mangling for the BLAS and LAPACK (we're
;;;; not writing a general Fortran name mangler), this should suffice
;;;; for our purposes.
;;;;
;;;; We explain in the comments below how CFFI is not sufficient for
;;;; our requirements; we include a C library which wraps the
;;;; necessary shared library functionality.  The C library must be
;;;; compiled and built into a shared library.


(in-package :lisp-matrix)


;;; Possible name mangling transformations.  NOTE: not all of them are
;;; orthogonal: e.g. appending one underscore vs. appending two
;;; underscores.

;; string-upcase, string-downcase, string-capitalize

;;(asdf:oos 'asdf:load-op 'cffi)

(defun prepend-one-underscore (s)
  (concatenate 'string "_" s))
(defun append-one-underscore (s)
  (concatenate 'string s "_"))
(defun append-two-underscores (s)
  (concatenate 'string s "__"))


;;; FIXME: change the name of the shared libraries to suit your needs!
;; First open libdl
(cffi:use-foreign-library "libdl.so")
;; Now open our custom library
(cffi:use-foreign-library "./libshared.so")

;;; Shared library wrapper functions.
(cffi:defcfun ("open_library" %open-library) :pointer
  (library-name :string))
(cffi:defcfun ("close_library" %close-library) :int
  (library :pointer))
(cffi:defcfun ("probe_library" %probe-library) :int
  (function-name :string)
  (library :pointer))

(defun probe-library (function-name library)
  (/= 0 (%probe-library function-name library)))

(defmacro with-shared-library ((library library-name) &body body)
  "Opens the shared library named LIBRARY-NAME and binds a handle
   to it to the variable LIBRARY, then executes BODY.

   We don't use CFFI for the following reasons:

   1. CFFI loads all objects into a common namespace, whereas
      we want a specific handle to a specific library.
   2. CFFI doesn't currently handle closing libraries very well,
      as of 31 Dec 2006 (the CLOSE-FOREIGN-LIBRARY function is
      deliberately not exported).

   Instead of using CFFI, we call our own custom C functions.
   For POSIX-compliant systems, these are wrappers for the POSIX
   DL functions (dlopen, dlclose, dlsym).  

   Because we aren't using CFFI, the only way to access the opened
   foreign library is via PROBE-LIBRARY or a wrapper function for 
   DLSYM; the pointer returned by the latter can be used in 
   FOREIGN-FUNCALL, if your CFFI and Lisp support FOREIGN-FUNCALL.
   "
  `(let ((,library nil))
    (unwind-protect
	 (progn
	   (setf ,library (%open-library ,library-name))
	   (assert (not (cffi:null-pointer-p ,library)))
	   ,@body)
      (if (and ,library (not (cffi:null-pointer-p ,library)))
	  (%close-library ,library)))))

(defun find-f77-mangling (function-name library-name)
  "Finds the Fortran 77 name mangling for the given function name,
   which is in the given shared library.  Returns NIL if the mangling
   cannot be found."
  (let ((capitalizations '(identity string-upcase 
			   string-downcase string-capitalize))
	(modifications '(identity prepend-one-underscore 
			 append-one-underscore append-two-underscores)))
    (with-shared-library (library library-name)
      (loop for capit in capitalizations do
	    (loop for modif in modifications 
		  do
		  (let ((mangled-name (funcall (symbol-function modif)
					       (funcall (symbol-function capit)
							function-name))))
		    (when (probe-library mangled-name library)
		      (return (list capit modif)))))))))

(defun get-f77-mangling-function (test-function-name library-name)
  (let ((pair (find-f77-mangling test-function-name library-name)))
    (if (null pair)
	(error "Failed to determine F77 name mangling function")
	(destructuring-bind (capit modif) pair
	  #'(lambda (s) (funcall (symbol-function modif)
				 (funcall (symbol-function capit)
					  s)))))))
