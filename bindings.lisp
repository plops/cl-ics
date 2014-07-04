(in-package :ics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-ics-libraries))

;; function definitions

(defcfun (%get-lib-version "IcsGetLibVersion") :string)

(defcfun (%version "IcsVersion") :int
  (filename :string)
  (forcename :int))


(defcenum ics-error
  (:err-ok 0)
  :err-filesize-conflict
  :err-output-not-filled
  :err-alloc)

(defctype :sizet :unsigned-int) ;; FIXME this might be necessary to be
				;; grovelled

(defcfun (%load-preview "IcsLoadPreview") ics-error
  (filename :string)
  (plane-number :sizet)
  (dst :pointer)
  (w (:pointer :sizet))
  (h (:pointer :sizet)))


(defcfun (%free "free") :void (ptr :pointer))

(defcfun (%open "IcsOpen") ics-error
  (ics :pointer)
  (filename :string)
  (mode :string))

(defcfun (%close "IcsClose") ics-error
  (ics :pointer))

(defcenum ics-datatype
  (:ics-unknown 0)
  :uint8
  :sint8
  :uint16
  :sint16
  :uint32
  :sint32
  :real32
  :real64
  :complex32
  :complex64)

(defcfun (%set-layout "IcsSetLayout") ics-error
  (ics :pointer)
  (datatype ics-datatype)
  (ndims :int)
  (dims :pointer))

(gethash :complex32 (cffi::keyword-values (funcall (cffi::find-type-parser 'ics-datatype))))

(defun set-layout (ics datatype dimensions)
  (%set-layout ics datatype ))
