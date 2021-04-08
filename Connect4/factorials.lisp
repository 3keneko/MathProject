(defun weird-factorial (a &optional (buffer 1))
  (flet ((writer (x) (setf buffer (* buffer x))))
    (%weird-factorial a #'writer))
  buffer)

(defun %weird-factorial (a writer)
  (unless (= a 0)
    (funcall writer a)
    (%weird-factorial (1- a) writer)))
