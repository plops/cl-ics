(declaim (optimize (speed 0) (safety 3) (debug 3)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/martin/stage/cl-ics/"
	  ))
  (asdf:load-system "ics"))

(defpackage :ics-test
  (:use :cl :ics))
(in-package :ics-test)


(let ((a (make-array (list 40 30) :element-type 'single-float
		     :initial-element 1f0)))
  (dotimes (j 40)
    (dotimes (i 30)
      (setf (aref a j i) (* 1f0 i))))
  (ics:write-ics2 "/dev/shm/o.ics" a))

