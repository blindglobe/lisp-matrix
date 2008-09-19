;;;; features.lisp
;;;; Author: mfh
;;;; Last modified: 15 Oct 2006
;;;;
;;;; Management of features that are specific to each Lisp
;;;; implementation.
;;;;

;;; Supported features.  These are keywords, and could include 
;;; the following:
;;;
;;; :can-pause-gc  -- the GC can be paused during the execution of a form.
;;; :static-arrays -- arrays can be "pinned" in memory on creation.
;;; :finalization -- a function can be called on GC of an object.
;;; :use-lisp-arrays -- we're using Lisp arrays and not C arrays
;;; 
;;; We start with NIL and push features on depending upon which
;;; Lisp implementation we are using.
(defparameter *our-features* nil)

(defun add-feature (feature)
  (push feature *our-features*))

(defun add-features (&rest features)
  "Adds the given FEATURES to the features list."
  (loop for f in features do (add-feature f) finally (return nil)))

;;; FIXME: These lists aren't all complete.  In particular, I don't
;;; have documentation for SCL.

#+allegro
(add-features :use-lisp-arrays :static-arrays :finalization)
#+clisp
(add-features :finalization)
#+(or cmu sbcl)
(add-features :can-pause-gc :finalization)
#+corman
(add-features :finalization)
#+ecl
(add-features :finalization)
#+gcl
(add-features :use-lisp-arrays :can-pause-gc :static-arrays :finalization)
#+lispworks
(add-features :use-lisp-arrays :static-arrays :can-pause-gc :finalization)
#+openmcl
(add-features :can-pause-gc :finalization)
#+scl
(add-features :finalization) ; I don't know what other features should be here
#-(or allegro clisp cmu corman ecl gcl lispworks openmcl sbcl scl)
(error "I don't know anything about your Lisp and I don't want to assume anything either.  You should add an entry here for your own Lisp, filling in the features that it implements.")

(defun has-feature-p (feature)
  "Returns non-nil iff our Lisp implementation implements the given FEATURE."
  (member feature *our-features*))
