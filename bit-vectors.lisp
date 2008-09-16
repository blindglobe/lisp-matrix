;;;; Author: mfh
;;;; Date: 1 Jan 2007
;;;; Last modified: 1 Jan 2007
;;;;
;;;; Fun with bit vectors!

;;; Functions for working with 'em.

(in-package :lisp-matrix)

(defun bvinc! (bv)
  "an in-place cascade bit incrementer, assuming that the least
signficant bit is (bit bv 0)."
  (let ((L (array-dimension bv 0)))
    (cond ((= L 0)
	   bv)
	  (t
	   (loop with c = 1 for i from 0 upto (1- L) do
		 (psetf (bit bv i) (logxor (bit bv i) c)
			c          (logand (bit bv i) c))
		 finally (return bv))))))


(defun bvinc1! (bv)
  "same as above, but (bit bv 0) is the most significant bit."
  (let ((L (array-dimension bv 0)))
    (cond ((= L 0)
	   bv)
	  (t
	   (loop with c = 1 for i from (1- L) downto 0 do
		 (psetf (bit bv i) (logxor (bit bv i) c)
			c          (logand (bit bv i) c))
		 finally (return bv))))))


(defun bv2int (bv)
  "converts the given bit vector to an integer, assuming that the
least significant bit is (bit bv 0)."
  (let ((L (array-dimension bv 0)))
    	(cond ((= L 0) 0)
	      (t
		(loop with x = 0 
		      with p = 1
		      for i from 0 upto (1- L) do
		      (incf x (* (bit bv i) p))
		      (setf p (* 2 p))
		      finally (return x))))))

(defun bv2int1 (bv)
  "same as above except (bit bv 0) is msb."
  (let ((L (array-dimension bv 0)))
    	(cond ((= L 0) 0)
	      (t
		(loop with x = 0 
		      with p = 1
		      for i from (1- L) downto 0 do
		      (incf x (* (bit bv i) p))
		      (setf p (* 2 p))
		      finally (return x))))))
