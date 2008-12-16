;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2007--2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is part of the unittests package.   See unittests.lisp for
;;; general philosophy.

;; (asdf:oos 'asdf:compile-op 'lift :force t)
;; (asdf:oos 'asdf:load-op 'lift)
;; (asdf:oos 'asdf:compile-op 'lisp-matrix)
;; (asdf:oos 'asdf:load-op 'lisp-matrix)

(in-package :lisp-matrix-unittests)

;; EVERYTHING
;; (run-lisp-matrix-tests)
;; (describe (run-lisp-matrix-tests))

;; VECTOR TESTS
;; (run-tests :suite 'lisp-matrix-ut-vectors)
;; (describe (run-tests :suite 'lisp-matrix-ut-vectors))
;; (run-test :test-case '   :suite 'lisp-matrix-ut-vectors)

;; REMINDER IF NEEDED
;; (remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

;;; TEST SUITES in file.

(deftestsuite lisp-matrix-ut-datatrans      (lisp-matrix-ut) ())

;;; SUPPORT FUNCTIONS

;; (in general, see unittests.lisp; any specific to vectors would be here...)

;;; TESTS: VECTORS

(addtest (lisp-matrix-ut-datatrans)
  list-to-vector-like
  (for-all-implementations
    (ensure (m= (make-vector 3 :initial-element 0d0)
		(list->vector-like (list 0d0 0d0 0d0))))))




