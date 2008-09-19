

;;; It's easier to unroll a DOTIMES than it is to unroll a general
;;; LOOP or ITERATE macro.  DOTIMES looks like
;;; (DOTIMES (var bound [optional-return-value]) &body)
;;;
;;; FIXME: what about declarations in the body?  Those will break 
;;; the code below.
(defmacro unroll-dotimes (the-dotimes)
  (destructuring-bind (var bound &optional retval) 
      (cadr the-dotimes)
    (assert (constantp bound) "The bound in DOTIMES must be a constant in order to apply UNROLL-DOTIMES to it.")
    (let ((unrolled-body
	   `(progn
	      ,(loop for i from 0 to #.(1- bound) appending
		     (list* @,body (incf ,var))))))
      `(let ((,var 0))
	 (declare (type ,var fixnum))
	 ,unrolled-body))))
      
  
  
  



;;; LU factorization code generator.  Generates LU factorization
;;; routines that factor the matrix in place using partial pivoting.
;;; The goal is to produce fully unrolled factorization routines, 
;;; though that can increase code size a lot.  For now we just 
;;; produce loops.
(defmacro generate-lu!-factorization (matsize)
  (let ((macro-name (format "lu~Ax~A!" ,matsize ,matsize)))
    `(defmacro ,macro-name (A pivarray)
       `(dotimes (j #.,matsize)
	  (declare (type j fixnum))
	  (declare (type ,pivarray (simple-array fixnum #.,matsize)))
	  ;; Find max pivot
	  (let ((pivrow
		 (loop with m = (aref ,A i j) 
		     with pivrow = j of-type fixnum
		     for i of-type fixnum from (1+ j) to #.(1- ,matsize) 
		     finally (return pivrow)
		     do
		       (let ((m2 (abs (aref ,A i j))))
			 (when (> m2 m)
			   (setv m m2)
			   (setv pivrow i))))))
	    (progn
	      (setf (aref ,pivarray j) pivrow)
	      ;; Swap row j and row pivrow
	      (when (/= pivrow j)
		(loop for i of-type fixnum from j to #.(1- ,matsize) do
		      (let ((temp (aref ,A j i)))
			(setf (aref ,A j i) (aref ,A pivrow i))
			(setf (aref ,A pivrow i) temp))))
	      ;; Assume that the pivot is nonzero and scale.
	      (let ((pivot (aref ,A j j)))
		(loop for i from (1+ j) to #.(1- ,matsize) do
		      (setf (aref ,A i j) (/ (aref ,A i j) pivot))))
	      ;; Update the remaining submatrix.
	      (loop for i of-type fixnum from (1+ j) to #.(1- ,matsize) do
		    (loop for k of-type fixnum from (1+ j) to #.(1- ,matsize) do
			  (setf (aref ,A i k)
			    (- (aref ,A i k) (* (aref ,A i j) (aref ,A j k))))))
	      ;; Return a list whose first element is the factored matrix
	      ;; A, and whose second element is the pivot array.  This avoids
	      ;; the need for multiple-value-bind, and the LU solver macro can
	      ;; destructure the list itself.
	      (list ,A ,pivarray)))))))

(dolist (s *fixed-sizes*)
  (generate-lu!-factorization s))

(defmacro generate-lu!-solver (matsize)
  (let ((macro-name (format nil "lusolve~Ax~A!" ,matsize ,matsize)))
    `(defmacro ,macro-name (x (LU pivarray) b)
       `(progn
	  ;; Apply the permutation to b, storing the result in x.
	  (loop for i of-type fixnum from 0 to #.(1- ,matsize)
	      do (setf (aref ,x i) (aref ,b (aref ,pivarray i))))
	  ;; Solve PLc = Pb, storing the result in x.
	  (loop for i of-type fixnum from 0 to #.(1- ,matsize)
	      do (loop for j of-type fixnum from 0 to (1- i)
		     do (setf (aref ,x i) (- (aref ,x i) (* (aref ,LU i j) (aref ,x j))))))
	  ;; Solve Ux = c, storing the result in x.
	  (loop for i of-type fixnum from #.(1- ,matsize) downto 0 do
		(loop for j of-type fixnum from #.(1- ,matsize) downto (1+ i) do
		      (setf (aref ,x i) (- (aref ,x i) (* (aref ,LU i j) (aref ,x j)))))
		(setf (aref ,x i) (/ (aref ,x i) (aref ,LU j j))))
	  ;; Return x.
	  ,x))))
					      
(dolist (s *fixed-sizes*)
  (generate-lu!-solver s))



;;;; Now you can say (lusolve4x4! x (lu4x4! A pivarray) b) and it returns the right answer (???).
