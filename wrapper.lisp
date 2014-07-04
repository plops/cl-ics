(in-package :ics)


(defun version (filename &optional (append-extension t))
  (%version filename (if append-extension 0 1)))
