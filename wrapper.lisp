(in-package :ics)

(defmacro with-check (&body body)
  (let ((err (gensym)))
    `(let ((,err ,@body))
       (unless (eq :err-ok #+nil (cffi-get-value 'ics-error :err-ok) ,err)
	 (error "error in ~a: \"~a\"" ',@body (%get-error-text ,err)
		))
       ,err)))

(defun version (filename &optional (append-extension t))
  (%version filename (if append-extension 0 1)))

(defun load-preview (filename &optional (plane-number 0))
  (with-foreign-objects ((w 'sizet)
			 (h 'sizet)
			 (dst :pointer))
    (with-check
     (%load-preview filename plane-number dst w h))
    (let* ((hh (mem-ref h 'sizet))
	   (ww (mem-ref w 'sizet))
	   (d (mem-ref dst :pointer))
	   (a (make-array (list hh ww) :element-type '(unsigned-byte 8))))
      (dotimes (j hh)
	(dotimes (i ww)
	 (setf (aref a j i) (mem-aref d :uint8 (+ i (* j ww))))))
      (%free d)
      a)))

(defun ics-open (filename mode)
  (with-foreign-object (ics :pointer)
    (with-check
     (%open ics filename mode))
    (mem-ref ics :pointer)))

(defun cffi-get-value (datatype keyword)
  (gethash keyword (cffi::keyword-values (funcall (cffi::find-type-parser datatype)))))

#+nil
(cffi-get-value 'ics-datatype :complex32)

(defun cffi-get-keyword (datatype value)
  (gethash value (cffi::value-keywords (funcall (cffi::find-type-parser datatype)))))

#+nil
(cffi-get-keyword 'ics-datatype 9)



(defun set-layout (ics datatype dimensions)
  (let ((n (length dimensions)))
   (with-foreign-object (dims 'sizet n) ;; fixme this doesn't work, see write-ics2 for a working version
     (let ((r (reverse dimensions)))
      (dotimes (i n)
	(setf (mem-aref dims 'sizet i) (elt r i))))
     #+nil (break (format nil "layout ~A" (list dimensions
					  (loop for i below n collect
					       (mem-aref dims 'sizet i)))))
     (with-check
	 (%set-layout ics datatype n dims)))))

(defun set-compression (ics &optional (compression :compression-uncompressed)
			      (level 0))
  (with-check
    (%set-compression ics compression level)))

(defun set-data (ics src)
  "src must be an array with array-storage-vector and must be pinned
until %close is called."
  (let ((el-size (let ((typ (array-element-type src)))
		   (cond 
		     ((equal typ 'single-float) 4)
		     ((equal typ 'double-float) 8)
		     ((equal typ '(complex single-float)) 8)
		     ((equal typ '(complex double-float)) 16)))))
    (with-check
      (%set-data ics (sb-sys:vector-sap (sb-ext:array-storage-vector src))
		 (* el-size (array-total-size src))))))


(defun ics-close (ics)
  (with-check
    (%close ics)))

(defun write-ics2 (filename data)
  (let (ics
	(datatype (let ((typ (array-element-type data)))
		    (cond 
		      ((equal typ 'single-float) :real32)
		      ((equal typ 'double-float) :real64)
		      ((equal typ '(complex single-float)) :complex32)
		      ((equal typ '(complex double-float)) :complex64))))
	(dims (make-array (array-rank data)
			  :element-type '(unsigned-byte 64)
			  :initial-contents (reverse (array-dimensions data)))))
    (sb-sys:with-pinned-objects (data dims)
      (unwind-protect
	   (progn
	     (setf ics (ics-open filename "w2"))
	     (%set-layout ics datatype (array-rank data) (sb-sys:vector-sap dims))
	     (set-compression ics)
	     (set-data ics data)) 
	(ics-close ics)))))
