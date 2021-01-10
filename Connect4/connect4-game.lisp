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

(defmacro get-pos (column row board)
  `(nth ,row ,(aref board column)))

(defun show-board (board)
  (let ((row nil))
        (loop for i from 1 to 6
            do (progn
                    (setf row (map 'list #'car board))
                    (format t "~{~A ~}|~D~%"
                            (mapcar #'playerify row) i)
                    (setf board (map 'vector #'cdr board)))
            finally (format t "~{~D ~}~%"
                            (count-to 7)))))

(defun fullp (i board)
  (car (aref board i)))

(defun tiedp (board)
  (notany #'null
          (map 'list #'car board)))

(defun play (i color board)
  (labels ((push-until (lat token)
             (cond
               ((eq 1 (length lat)) `(,token))
               ((cadr lat) (cons token (cdr lat)))
               (t (cons (car lat)
                      (push-until (cdr lat) token))))))
    (setf (aref board i) (push-until (aref board i) color))
    board))

(defun maximum (lat)
  (reduce (lambda (a b)
            (if (> a b)
                a
                b))
          lat))

(defun max-number-of-vertical-connections (color board)
  (labels ((number-in-vertical (column colour &optional (buffer 0))
             (cond
               ((null column) buffer)
               ((not (car column)) (number-in-vertical (cdr column) colour 0))
               ((eq (car column) colour)
                (number-in-vertical (cdr column) colour (1+ buffer)))
               (t buffer))))
    (maximum (map 'list (lambda (column)
                          (number-in-vertical column color))
                  board))))

(defun max-subseq (color seq)
  (let ((maxi 0)
        (temp 0))
    (loop for token in seq
          if (eq color token)
            do (progn
                 (incf temp)
                 (setf maxi (max temp maxi)))
          else
            do (setf temp 0))
    maxi))

(defun max-number-of-horizontal-connections (color board)
  (labels ((row-finder (the-board)
             (loop for row from 0 to 5
                   collect (map 'list #'car the-board) into rows
                   do (setf the-board (map 'vector #'cdr the-board))
                   finally (return rows))))
    (maximum (mapcar (lambda (row)
                       (max-subseq color row))
                     (row-finder board)))))


(defun make-left-diagonal (the-case
                &optional (max-row 5))
  (let ((column (car the-case))
        (row    (cdr the-case)))
    (loop for c from column downto 0
          for r from row to max-row
          collect (cons c r))))

(defun make-right-diagonal (the-case
                            &optional
                              (max-row 5)
                              (max-column 6))
  (let ((column (car the-case))
        (row    (cdr the-case)))
    (loop for c from column to max-column
          for r from row to max-row
          collect (cons c r))))

(defparameter *top-right-corner* '((3 . 0) (4 . 0) (5 . 0)
                                   (6 . 0) (6 . 1) (6 . 2)))

(defparameter *top-left-corner* '((3 . 0) (2 . 0) (1 . 0)
                                  (0 . 0) (0 . 1) (0 . 2)))

(defmacro max-in-some-diagonal
    (function-name color board helping-function starting-corner)
  `(defun ,function-name (,color ,board)
     (let* ((diagonals (mapcar #',helping-function ,starting-corner))
            (diag-values (mapcar
                          (lambda (diags)
                            (mapcar
                             (lambda (coords)
                               (let ((column (car coords))
                                     (row (cdr coords)))
                                 (nth row (aref ,board column))))
                             diags))
                          diagonals)))
       (maximum (mapcar
                 (lambda (diagonal)
                          (max-subseq ,color diagonal))
                 diag-values)))))

(max-in-some-diagonal max-number-of-left-diagonal-connections
                      color board  make-left-diagonal *top-right-corner*)
(max-in-some-diagonal max-number-of-right-diagonal-connections
                      color board make-right-diagonal *top-left-corner*)

(defun max-number-of-connections (color board)
  (max
   (max-number-of-vertical-connections       color board)
   (max-number-of-horizontal-connections     color board)
   (max-number-of-left-diagonal-connections  color board)
   (max-number-of-right-diagonal-connections color board)))

(defun winningp (board) 
  (or (> (max-number-of-connections 'yellow board) 3)
      (> (max-number-of-connections 'red    board) 3)))

(defun play-repl ()
  (loop while (and (not (winningp *board*))
                   (not (tiedp *board*)))
        do (progn
             (if (winningp *board*)
                 (return "Le joueur 2 a gagné."))
             (format t "Au tour du joueur 1!~%~A~%Dans quelle colonne souhaitez-vous jouer? "
                     (show-board *board*))
             (let* ((column (read))
                    (i (1- column)))
               (cond
                 ((not (<= 1 column 7))
                  (format t "Veuillez jouer un numéro de colonne correct!~%")
                  (play-repl))
                 ((fullp i *board*)
                  (format t "Cette colonne est déjà remplie!~%")
                  (play-repl))
                 (t (setf *board* (play i 'yellow *board*))))))
        do (progn
             (if (winningp *board*)
                 (return "Le joueur 1 a gagné."))
             (format t "Au tour du joueur 2!~%~A~%Dans quelle colonne souhaitez-vous jouer? "
                     (show-board *board*))
             (let* ((column (read))
                    (i (1- column)))
               (cond
                 ((not (<= 1 column 7))
                  (format t "Veuillez jouer un numéro de colonne correct!~%")
                  (play-repl))
                 ((fullp i *board*)
                  (format t "Cette colonne est déjà remplie!~%")
                  (play-repl))
                 (t (setf *board* (play i 'red *board*))))))))
             ;;;(setf *board* (computer-turn 'red *board*)))))
