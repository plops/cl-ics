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

