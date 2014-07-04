(declaim (optimize (debug 3) (safety 3) (speed 0)))


(setf asdf:*central-registry*
      '(*default-pathname-defaults*
	#p"/home/martin/stage/cl-ics/"
    ))
#+nil
(asdf:load-system "ics")
