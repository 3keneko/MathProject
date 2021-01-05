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
  (labels ((push-until (lat token)
           (when (every #'null lat)
             (setf (sixth lat) token)
             lat)
           (cond
             ((cadr lat) (cons token (cdr lat)))
             (t (cons (car lat)
                      (push-until (cdr lat) token))))))
    (push-until (aref board i) color)))

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
               ((not (car column)) (number-in-vertical (cdr column) 0))
               ((eq (car column) colour)
                (number-in-vertical (cdr column) (1+ buffer)))
               (t buffer))))
    (maximum (map 'list (lambda (column)
                          (number-in-vertical column color))
                  board))))

(defun max-subseq (color seq)
  (let ((maxi 0)
        (temp 0))
    (loop for token across seq
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

(defun max-number-of-nw-diagonal-connections (color board)
  (labels ((nw-descent (v-board column row)
             (let ((acc nil))
                (loop while (and (/= (1- column) -1)
                                 (/= (1+    row)  6))
                      do (progn
                           (setf acc (cons (get-pos column row v-board)
                                           acc))
                          (decf column)
                          (incf row)))
                  acc))
           (fetch-from-column (v-board column row)
             (let ((tracker 0)
                   (base-column column)
                   (acc nil))
               (loop while (< tracker 6)
                     if (or (< (1-    row) 0)
                            (> (1+ column) 6))
                       do (progn
                            (setf acc
                              (cons (nw-descent v-board column row) acc))
                            (decf tracker)
                            (setf row tracker column base-column))
                     else do (progn
                               (decf row)
                               (incf column)))
               acc)))
  (max-subseq color (fetch-from-column board 3 0))))
             
(defun max-number-of-ne-diagonal-connections (color board)
  (labels ((no-descent (v-board column row)
             (let ((acc nil))
               (loop while (and (/= (1+ column) 7)
                                (/= (1+    row) 6))
                     do (progn
                          (setf acc (cons (get-pos column row v-board)
                                          acc))
                          (incf column)
                          (incf row)))
               acc))
           (fetch-from (v-board column row)
             (let ((tracker 0)
                   (base-column column)
                   (acc nil))
               (loop while (< tracker 6)
                     if (or (< (1- row) 0)
                            (< (1- column) 0))
                       do (progn
                            (setf acc
                                  (cons (no-descent v-board column row) acc))
                            (decf tracker)
                            (setf row tracker column base-column))
                     else do (progn
                               (decf row)
                               (decf column)))
               acc)))
    (max-subseq color (fetch-from board 3 0))))

(defun max-number-of-connections (color board)
  (max
   (max-number-of-vertical-connections color board)
   (max-number-of-horizontal-connections color board)
   (max-number-of-ne-diagonal-connections color board)
   (max-number-of-nw-diagonal-connections color board)))

(defun winningp (board) 
  (or (> (max-number-of-connections 'yellow board) 3)
      (> (max-number-of-connections 'red    board) 3)))

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
                 (t (setf *board* (play i 'yellow *board*))))))
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
                 (t (setf *board* (play i 'yellow *board*))))))))
             ;;;(setf *board* (computer-turn 'red *board*)))))
