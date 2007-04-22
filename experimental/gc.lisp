;;;; gc.lisp
;;;; Author: mfh
;;;;
;;;; Macros and functions dealing with manipulating the GC.
;;;; Needs: features.lisp
;;;;


(defmacro with-gc-off (&body body)
  "If your Lisp supports it, pauses the GC during execution
   of BODY, and restores it afterwards."
  #+cmucl
  `(sys:without-gcing ,@body)
  #+sbcl
  `(sb-sys:without-gcing ,@body)
  #-(or cmucl sbcl)
  (let ((stop-gc-expr
	 #+gcl
	 `(si:sgc-on nil)
	 #+openmcl
	 `(egc nil)
	 #+lispworks
	 (progn
	   (warn "AVOID-GC in LispWorks isn't guaranteed to prevent a GC")
	   `(avoid-gc))
	 #-(or gcl openmcl lispworks)
	 (error "I don't know how to turn off the GC in your Lisp"))
	(start-gc-expr
	 #+gcl
	 `(si:sgc-on t)
	 #+openmcl
	 `(egc t)
	 #+lispworks
	 (normal-gc)
	 #-(or gcl openmcl lispworks)
	 (error "I don't know how to turn on the GC in your Lisp")))
    `(unwind-protect
	 (progn
	   ,stop-gc-expr
	   ,@body)
       ,start-gc-expr)))

(defun must-pause-gc ()
  "Returns T iff we have to pause GC when passing one of our arrays
   (not any old user array) into a foreign function."
  (and (has-feature-p :can-pause-gc)
       (not (has-feature-p :static-arrays))
       (has-feature-p :use-lisp-arrays)))

(defmacro with-array-protection (&body body)
  "If needed, protects arrays from garbage collection during the
   execution of BODY.  If not needed, wraps BODY in a PROGN."
  (if (must-pause-gc)
      `(with-gc-off
	   ,@body)
    `(progn
       ,@body)))

