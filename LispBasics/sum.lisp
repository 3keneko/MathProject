(defun f (n)
  (let ((buffer 0))
    (loop for i from 1 to n
          do (incf buffer i))
    buffer))

(defmacro let-in (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defun g (n)
  (let-in buffer 0
    (loop for i from 1 to n
          do (incf buffer i))
    buffer))
