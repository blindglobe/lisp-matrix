;;; -*- mode: lisp -*-
;;; Copyright (c) 2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; This is semi-external to lisp-matrix core package.  The dependency
;;; should be that lisp-matrix packages are dependencies for the unit
;;; tests.  However, where they will end up is still to be
;;; determined. 

(in-package :cl-user)

(defpackage :lisp-matrix-unittests
  (:use :common-lisp :lift :lisp-matrix)
  (:export run-lisp-matrix-tests))

(in-package :lisp-matrix-unittests)

;;; TESTS

(defun run-lisp-matrix-tests ()
  (run-tests :suite 'lisp-matrix-ut))

;; (run-lisp-stat-tests)


(defun run-lisp-matrix-test (&rest x)
  (run-test x))


(deftestsuite lisp-matrix-ut () ())



(deftestsuite lisp-matrix-ut-matrix-foreign (lisp-matrix-ut) ())



;;; lisp-arrays
(deftestsuite lisp-matrix-ut-matrix-lisp (lisp-matrix-ut) ())

(addtest
    (lisp-matrix-ut-matrix-lisp) create-double-matrix
    (ensure-same 
     (log-gamma 3.4)
     1.0923280596789584 ))


;;; Normal distribution

(deftestsuite lisp-stat-ut-probdist-f (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Gaussian distn results")
  (:test (ensure-same
	  (normal-quant 0.95)
	  1.6448536279366268))
  (:test (ensure-same
	  (normal-cdf 1.3)
	  0.9031995154143897))
  (:test (ensure-same
	  (normal-dens 1.3)
	  0.17136859204780736))
  (:test (ensure-same
	  (normal-rand 2)
	  (list -0.40502015f0 -0.8091404f0)))
  (:test (ensure-same
	  (bivnorm-cdf 0.2 0.4 0.6)
	  0.4736873734160288)))

;;;; Cauchy distribution

(deftestsuite lisp-stat-ut-probdist-cauchy (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Cachy-distn results")
  (:test (ensure-same
	  (cauchy-quant 0.95)
	  6.313751514675031))
  (:test (ensure-same
	  (cauchy-cdf 1.3)
	  0.7912855998398473))
  (:test (ensure-same
	  (cauchy-dens 1.3)
	  0.1183308127104695 ))
  (:test (ensure-same
	  (cauchy-rand 2)
	  (list -1.06224644160405 -0.4524695943939537))))

;;;; Gamma distribution

(deftestsuite lisp-stat-ut-probdist-gamma (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for gamma distn results")
  (:test (ensure-same
	  (gamma-quant 0.95 4.3)
	  8.178692439291645))
  (:test (ensure-same
	  (gamma-cdf 1.3 4.3)
	  0.028895150986674906))
  (:test (ensure-same
	  (gamma-dens 1.3 4.3)
	  0.0731517686447374))
  (:test (ensure-same
	  (gamma-rand 2 4.3)
	  (list 2.454918912880936 4.081365384357454))))

;;;; Chi-square distribution

(deftestsuite lisp-stat-ut-probdist-chisq (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for Chi-square distn results")
  (:test (ensure-same
	  (chisq-quant 0.95 3)
	  7.814727903379012))
  (:test (ensure-same
	  (chisq-cdf 1 5)
	  0.03743422675631789))
  (:test (ensure-same
	  (chisq-dens 1 5)
	  0.08065690818083521))
  (:test (progn
	   (set-seed 353)
	   (ensure-same
	    (chisq-rand 2 4)
	    (list 1.968535826180572 2.9988646156942997)))))

;;;; Beta distribution

(deftestsuite lisp-stat-ut-probdist-beta (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for beta distn results")
  (:test (ensure-same
	  (beta-quant 0.95 3 2)
	  0.9023885371149876))
  (:test (ensure-same
	  (beta-cdf 0.4 2 2.4)
	  0.4247997418541529 ))
  (:test (ensure-same
	  (beta-dens 0.4 2 2.4)
	  1.5964741858913518 ))
  (:test (ensure-same
	  (beta-rand 2 2 2.4)
	  (list 0.8014897077282279 0.6516371997922659))))

;;;; t distribution

(deftestsuite lisp-stat-ut-probdist-t (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for t-distn results")
  (:test (ensure-same
	  (t-quant 0.95 3)
	  2.35336343484194))
  (:test (ensure-same
	  (t-cdf 1 2.3)
	  0.794733624298342))
  (:test (ensure-same
	  (t-dens 1 2.3)
	  0.1978163816318102))
  (:test (ensure-same
	  (t-rand 2 2.3)
	  (list -0.34303672776089306 -1.142505872436518))))

;;;; F distribution

(deftestsuite lisp-stat-ut-probdist-f (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for f-distn results")
  (:test (ensure-same
	  (f-quant 0.95 3 5) 5.409451318117459))
  (:test (ensure-same
	  (f-cdf 1 3.2 5.4)
	  0.5347130905510765))
  (:test (ensure-same
	  (f-dens 1 3.2 5.4)
	  0.37551128864591415))
  (:test (progn
	   (set-seed 234)
	   (ensure-same
	    (f-rand 2 3 2)
	    (list 0.7939093442091963 0.07442694152491144)))))

;;;; Poisson distribution

(deftestsuite lisp-stat-ut-probdist-poisson (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for poisson distribution results")
  (:test (ensure-same
	  (poisson-quant 0.95 3.2) 6))
  (:test (ensure-same
	  (poisson-cdf 1 3.2)
	  0.17120125672252395))
  (:test (ensure-same
	  (poisson-pmf 1 3.2)
	  0.13043905274097067))
  (:test (progn
	   (set-seed 234)
	   (ensure-same
	    (poisson-rand 5 3.2)
	    (list 2 1 2 0 3)))))

;; Binomial distribution

(deftestsuite lisp-stat-ut-probdist-binomial (lisp-stat-ut-probdistn)
  ()
  (:documentation "testing for binomial distribution results")

  (:test (ensure-same
	  (binomial-quant 0.95 3 0.4) ;;; DOESN'T RETURN
	  ))
  (:test (ensure-same 
	  (binomial-quant 0 3 0.4)
	  ;; -2147483648
	  ))
  (:test (ensure-same
	  (binomial-cdf 1 3 0.4)
	  0.6479999999965776))
  
  (:test (ensure-same
	  (binomial-pmf 1 3 0.4)
	  0.4320000000226171))
  (:test (progn
	   (set-seed 526)
	   (ensure-same 
	    (binomial-rand 5 3 0.4)
	    (list 2 2 0 1 2)))))

