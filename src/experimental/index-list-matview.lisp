;;;; INDEX-LIST-MATVIEW

;;; No offsets here; the stored indices carry that information.
;;; The elements in the matview have indices
;;; (row-indices[0], col-indices[0]), 
;;; (row-indices[1], col-indices[1]),
;;; etc.  This means (= (length row-indices) (length col-indices)).
;;; It's like coordinate storage of a sparse matrix.
;;;
;;; nrows and ncols are useful for matrix-matrix multiplication 
;;; and the like.
(defclass index-list-matview (matview)
  ((row-indices :initform (error "Must supply a vector of row indices")
		:initarg :row-indices
		:reader row-indices)
   (col-indices :initform (error "Must supply a vector of column indices")
		:initarg :col-indices
		:reader col-indices)))

(defmethod initialize-instance :after ((A index-list-matview) &key)
  (with-slots (nrows ncols dim0 dim1 row-indices col-indices) A
    (assert (= (array-rank row-indices) 1))
    (assert (= (array-rank col-indices) 1))
    (assert (= (array-total-size row-indices) 
	       (array-total-size col-indices)))
    ;; We also check if the indices are in range.
    (loop 
	for i across row-indices
	for j across col-indices
        do (assert (and (>= i 0) (< i dim0)))
	   (assert (and (>= j 0) (< j dim1))))))

(defun apply-stride-to-indices (indices stride)
  "Returns a vector (indices[0], indices[0+stride], ...)"
  (let ((len (/ (array-total-size indices) stride)))
    (assert (integerp len))
    (if (= 0 len) 
	(make-array 0)
      (let ((result (make-array len :element-type (type-of (aref indices 0)))))
	(dotimes (i len result)
	  (when (= (floor i stride) (/ i stride))
	    (setf (aref result (floor i stride)) (aref indices i))))))))
  
;;; This is a special case: a strided subview of an index-list
;;; matview is an index-list matview.
(defmethod strided-subview ((A index-list-matview)
			    &key :nrows :ncols 
				 :offset0 :offset1 
				 :stride0 :stride1)
  (make-instance 'index-list-matview
    :my-array (slot-value A 'my-array)
    :nrows nrows
    :ncols ncols
    :dim0 (slot-value A 'dim0)
    :dim1 (slot-value A 'dim1)
    :row-indices (apply-stride-to-indices (slot-value A 'row-indices) stride0)
    :col-indices (apply-stride-to-indices (slot-value A 'col-indices) stride1)))
  
(defun indices-are-unit-stride-p (indices)
  "Returns T if the differences between consecutive elements in the given integer vector INDICES are all one."
  (block returning-place
    (let ((S (scan 'vector indices)))
      (collect-fn 'integer 
		  #'(lambda () t)  
		  ;; Short-circuiting check for all ones.
		  #'(lambda (prev cur) (if (/= cur 1)
					   (return-from returning-place nil)
					 t))
		  ;; Generate a series of differences between consecutive
		  ;; elements.  If all the elements in this series are one,
		  ;; the indices are unit-stride.
		  (map-fn 'integer #'- S 
			  (previous S (1- (collect-first S))))))))

(defmethod unit-stride-p ((A index-list-matview))
  (and (indices-are-unit-stride-p (slot-value A 'row-indices))
       (indices-are-unit-stride-p (slot-value A 'col-indices))))

(defmethod matview-index-to-1d-index ((A index-list-matview) i j)
  (with-slots (offset0 offset1 orientation 
	       dim0 dim1 row-indices col-indices) A
    (cond ((eql orientation :column)
	   (+ offset0 (aref row-indices i) 
	      (* dim0 (+ offset1 (aref col-indices j)))))
	  ((eql orientation :row)
	   (+ offset1 (aref col-indices j) 
	      (* dim1 (+ offset0 (aref row-indices i)))))
	  (t (error "Invalid orientation ~A" orientation)))))


(defmethod scan-matview ((A index-list-matview))
  (with-slots (my-array nrows ncols orientation 
	       dim0 dim1 row-indices col-indices) A
    (cond ((= 0 (* nrows ncols)) #Z())
	  ((eql orientation :column)
	   (producing (w) ((i 0) (j 0))
		      (loop 
			(tagbody
			  (when (>= j ncols)
			    (incf i))
			  (when (>= i nrows)
			    (terminate-producing))
			  (next-out w (aref my-array 
					    (aref row-indices i)
					    (aref col-indices j)))
			  (incf j)))))
	  ((eql orientation :row)
	   (producing (w) ((i 0) (j 0))
		      (loop 
			(tagbody
			  (when (>= i nrows)
			    (incf j))
			  (when (>= j ncols)
			    (terminate-producing))
			  (next-out w (aref my-array 
					    (aref row-indices i)
					    (aref col-indices j)))
			  (incf i)))))
	  (t (error "Invalid orientation ~A" orientation)))))
			   
(defmethod transpose ((A index-list-matview))
  (make-instance index-list-matview
    :my-array (slot-value A 'my-array)
    :nrows (slot-value A 'ncols)
    :ncols (slot-value A 'nrows)
    :orientation (opposite-orientation (slot-value A 'orientation))
    :dim0 (slot-value A 'dim1)
    :dim1 (slot-value A 'dim0)
    :offset0 (slot-value A 'offset1)
    :offset1 (slot-value A 'offset0)
    :row-indices (slot-value A 'col-indices)
    :col-indices (slot-value A 'row-indices)))


(defmethod restore ((OUT index-list-matview) (IN matrix)))


