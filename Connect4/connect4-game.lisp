(defparameter *board*
  (make-array 7 :initial-element
              (make-list 6 :initial-element nil)))

(defun playerify (board-case)
  (case board-case
    (yellow 'o)
    (red    'x)
    (t      '+))) 

(defmacro count-to (x)
  (let ((i (gensym)))
    `(loop for ,i from 1 to ,x
          collect ,i)))

(defun show-board (board)
  (let ((row nil))
        (loop for i from 1 to 5
            do (progn
                    (setf row (map 'list #'car board))
                    (format t "~{~A ~}|~D~%" row i)
                    (setf board (map 'vector #'cdr board)))
            finally (format t "~{~D ~}~%"
                            (count-to 7)))))
