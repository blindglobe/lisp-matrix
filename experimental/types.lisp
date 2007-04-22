;;;; types.lisp
;;;; Author: mfh
;;;;
;;;; Functions for conversion between Lisp types and CFFI types.
;;;; Needs: CFFI
;;;; 
;;;; FIXME: I should really use CFFI functions to do this, as the 
;;;; exact type conversions depend on the particular Lisp 
;;;; implementation.

;;; mfh 15 Oct 2006
;;;
;;; In CFFI, C structs cannot be passed by value.  This excludes
;;; passing C99 / Fortran complex numbers around unless we do some
;;; serious hackery.  In particular, defcstruct won't help us, nor
;;; will defcunion (as unions are implemented as structs in which all
;;; slots have an offset of zero).  On 15 Oct 2006 I e-mailed
;;; cffi-devel@common-lisp.net to ask how hard it would be for us to
;;; add complex float datatypes to CFFI.  As Lisp implementations
;;; themselves may not have complex float datatypes in their FFI's,
;;; this may require some bit twiddling (e.g. extracting an
;;; (unsigned-byte 128) and interpreting it as two double-floats).

(defpackage :types
  (:use :common-lisp)
  (:export :lisptype->cffi :cffitype->lisp))
(in-package :types)


(defun lisptype->cffi (type)
  "Given a Lisp floating-point (real or complex) value type,
   returns the corresponding CFFI type.  If given the CFFI 
   floating-point type, just returns that type."
  (cond ((or (eq type 'double-float) (eq type :double))
	 :double)
	((or (eq type 'single-float) (eq type :float))
	 :float)
	(t (error "Unrecognized type ~A" type))))

(defun cffitype->lisp (type)
  "Given a CFFI floating-point (real or complex) value type,
   returns the corresponding Lisp type.  If given the Lisp
   floating-point type, just returns that type."
  (cond ((or (eq type 'double-float) (eq type :double))
	 'double-float)
	((or (eq type 'single-float) (eq type :float))
	 'single-float)
	(t (error "Unrecognized type ~A" type))))

