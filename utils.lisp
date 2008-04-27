(in-package :lisp-matrix)

(defun make-symbol* (&rest args)
  "build a symbol by concatenating each element of ARGS, and intern it
  in the current package.  Elements can be strings or symbols."
  (intern (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (etypecase arg
                             (symbol (symbol-name arg))
                             (string arg)))
                         args))))
