
;;; This file is used to support interactive testing for LISP-MATRIX
;;; it manages to remind (me) how to stage the development.

(ql:quickload :lisp-matrix)

(in-package :lisp-matrix-unittests)

(describe (run-lisp-matrix-tests))           ; long summary
;; current: 69 tests, 2 errors


;;;; code for memory

;; (run-lisp-matrix-tests)                      ; quick summary
;;(remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)
;;(remove-test :test-case 'test-2 :suite 'lisp-matrix-ut-matrix-gemm)

;; EVERYTHING
;; (run-lisp-matrix-tests)
;; (describe (run-lisp-matrix-tests))

(describe (run-test :test-case 'bind2-dims-conditions))
(describe (run-test :test-case 'r-apply-columns))
(describe (run-test :test-case 'diagonalf-vectors))
(describe (run-test :test-case 'diagonal!-vectors))


;; VECTOR TESTS
;; (run-tests :suite 'lisp-matrix-ut-vectors)
;; (describe (run-tests :suite 'lisp-matrix-ut-vectors))
;; (run-test :test-case '   :suite 'lisp-matrix-ut-vectors)

;; REMINDER IF NEEDED
;; (remove-test :test-case 'data-initialize :suite 'lisp-matrix-ut)

