(defmacro let-in (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro randran (a b) 
  `(+ ,a (random (- ,b ,a))))

(defun main () 
  (let-in usr (read)
    (loop for number from 1 to usr
          do (format t "~A~%" (randran 1 11)))))

