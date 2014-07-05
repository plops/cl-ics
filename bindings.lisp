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

(defcfun (%get-error-text "IcsGetErrorText") :string
  (err ics-error))

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

;; (gethash :complex32 (cffi::keyword-values (funcall (cffi::find-type-parser 'ics-datatype))))


(defcfun (%set-layout "IcsSetLayout") ics-error
  (ics :pointer)
  (datatype ics-datatype)
  (ndims :int)
  (dims :pointer))

(defcfun (%set-data "IcsSetData") ics-error
  (ics :pointer)
  (src :pointer)
  (n :sizet))

(defcenum ics-compression
  (:compression-uncompressed 0)
  :compression-compress
  :compression-gzip)

(defcfun (%set-compression "IcsSetCompression") ics-error
  (ics :pointer)
  (compression ics-compression)
  (level :int))

