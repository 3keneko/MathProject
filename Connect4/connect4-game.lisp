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

(defun fullp (i board)
  (not (car (aref board i))))

(defun tiedp (board)
  (notany #'null
          (map 'list #'car board)))

(defun play (i color board)
  (flet ((push-until (lat token)
           (when (every #'null lat)
             (setf (sixth lat) token)
             lat)
           (cond
             ((cadr lat) (cons token (cdr lat)))
             (t (cons (car lat)
                      (push-until (cdr lat) token))))))
    (push-until (aref board i) color)))

(defun play-repl ()
  (loop while (and (not (winningp *board*))
                   (not (tiedp *board*)))
        do (progn
             (format t "A votre tour!~%Dans quelle colonne souhaitez-vous jouer? ")
             (let* ((column (read))
                    (i (1- column)))
               (cond
                 ((not (<= 1 column 7))
                  (format t "Veuillez jouer un numéro de colonne correct!~%")
                  (play-repl))
                 ((fullp i *board*)
                  (format t "Cette colonne est déjà remplie!~%")
                  (play-repl))
                 (t (setf *board* (play i 'yellow *board*)))))
             (setf *board* (computer-turn 'red *board*)))))
