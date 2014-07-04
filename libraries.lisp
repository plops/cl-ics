(in-package :ics)


(define-foreign-library ics
  (t (:default "/usr/local/lib/libics")))


(defun load-ics-libraries ()
  (use-foreign-library ics))
