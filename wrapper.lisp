(in-package :ics)


(defun version (filename &optional (append-extension t))
  (%version filename (if append-extension 0 1)))

(defun load-preview (filename &optional (plane-number 0))
  (with-foreign-objects ((w :sizet)
			 (h :sizet)
			 (dst :pointer))
    (%load-preview filename plane-number dst w h)
    (let* ((hh (mem-ref h :sizet))
	   (ww (mem-ref w :sizet))
	   (d (mem-ref dst :pointer))
	   (a (make-array (list hh ww) :element-type '(unsigned-byte 8))))
      (dotimes (j hh)
	(dotimes (i ww)
	 (setf (aref a j i) (mem-aref d :uint8 (+ i (* j ww))))))
      (%free d)
      a)))

(defun ics-open (filename mode)
  (with-foreign-object (ics :pointer)
    (%open ics filename mode)
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
   (with-foreign-object (dims :sizet n)
     (dotimes (i n)
       (setf (mem-aref dims :sizet i) (aref dimensions i)))
     (let ((err (%set-layout ics datatype n dims)))
      (unless (= (cffi-get-value 'ics-error :err-ok) err)
	(error "set-layut: \"~a\"" (%get-error-text err)))
      err))))
