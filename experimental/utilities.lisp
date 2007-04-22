

(defun array-in-place-transpose (A m n scratch)
  "A: array with m x n elements.
   m: number of columns in the original matrix.
   n: number of rows in the original matrix.
   scratch: scratch space array with space for n elements."
  (if (= m 1) 
      nil
    (progn
      ;; Copy the old first row into the scratch space, 
      ;; which stores the new first column.
      (dotimes (j n)
	(setf (aref scratch j) (aref A (* m j))))
      ;; Shift the remaining elements over to the right (starting
      ;; from the right side), thus leaving a space of length m
      ;; on the left side.
      (let ((c (1- (* m n))))
	(loop for j from (1- n) downto 0 do
	      (loop for i from (1- m) downto 1 do
		    (setf (aref A c) (aref A (+ i (* m j))))
		    (decf c))))
      ;; In that space we've created, put the new first column.
      (dotimes (j n)
	(setf (aref A j) (aref scratch j)))
      ;; Recurse on the remaining submatrix.
      (array-in-place-transpose ((make-array (- (* m n) m) 
					   :displaced-to A
					   :displaced-index-offset m)
			       (1- m) n scratch)))))


(defun find-in-tree (symb tree &optional (pred #'eq))
  "Finds the given object SYMB in the given tree TREE, using the equality test PRED."
  (if (null tree)
      nil
    (if (funcall pred symb tree)
	tree
      (or (find-in-tree symb (car tree) pred)
	  (find-in-tree symb (cdr tree) pred)))))

;;; Looks for a given symbol SYMB as the head of a list (i.e. in
;;; function position) in the structure of the given tree TREE.
;;; If found, returns the entire function call.
(defun find-function-in-tree (symb tree)	
  (if (atom tree) 
      nil
    (if (eq (car tree) symb)
	tree
      (or (find-function-in-tree (car tree))
	  (find-function-in-tree (cdr tree))))))
