(in-package :lisp-matrix)

(defmacro define-abstract-class (classname super-list &body body)
  "A wrapper for DEFCLASS that lets you define abstract base classes.
   If you try to instantiate an object of this class, a warning is signaled."
  `(progn
     (defclass ,classname ,super-list ,@body)

     ;; Protect against abstract class instantiation.

     ;; We could remove this programmatically later using a
     ;; compile-time constant (or even check the optimization options
     ;; and remove it if SAFETY is set low enough).
     (defmethod initialize-instance :before ((x ,classname) &key)
       (if (eql (type-of x) ',classname)
	   (warn "~A is an abstract base class and not to be instantiated." 
                 (quote ',classname))))))
