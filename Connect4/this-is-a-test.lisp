(defparameter *buffer* 1)
(defun weird-factorial (a)
  (unless (= a 0)
    (progn
      (setf *buffer* (* a *buffer*))
      (weird-factorial (1- a))))
  *buffer*)
