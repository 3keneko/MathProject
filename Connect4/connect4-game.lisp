(defparameter *board*
  (make-array 7 :initial-element
              (make-list 6 :initial-element nil))
  "Le plateau de puissance 4 en lui-même.")

(defun playerify (board-case)
  "Permet d'afficher proprememnt chaque case."
  (case board-case
    (yellow 'o)
    (red    'x)
    (t      '+))) 

(defmacro count-to (x)
  "Une macro similaire à iota de la bibliothèque alexandria."
  (let ((i (gensym)))
    `(loop for ,i from 1 to ,x
          collect ,i)))

(defmacro get-pos (column row board)
  "Retourne l'état de la case se trouvant à un certain
endroit sur le plateau."
  `(nth ,row ,(aref board column)))

(defun show-board (board)
  "Permet d'afficher le plateau."
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
  "Vérifie si une colonne est vide."
  (car (aref board i)))

(defun tiedp (board)
  "Vérifie si la position est nulle."
  (notany #'null
          (map 'list #'car board)))

(defun play (i color board)
  "Fonction permettant de jouer un jeton sur le plateau."
  (labels ((push-until (lat token)
             (cond
               ((eq 1 (length lat)) `(,token))
               ((cadr lat) (cons token (cdr lat)))
               (t (cons (car lat)
                      (push-until (cdr lat) token))))))
    (setf (aref board i) (push-until (aref board i) color))
    board))

(defun maximum (lat)
  "Donne le nombre maximum dans une liste simplement chaînée."
  (reduce (lambda (a b)
            (if (> a b)
                a
                b))
          lat))

(defun vertical-connections (color board)
  "Donne le nombre maximal de connection verticale."
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
  "Donne le nombre maximal d'élément se ressemblant dans une liste. 
e.g => (max-subseq 'red '(red red yellow red red red yellow yellow)) == 2"
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

(defun horizontal-connections (color board)
  "Donne le nombre maximal de connections horizontales."
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
  "Donne les diagonales allant d'en haut à gauche,
jusqu'à en bas à droite."
  (let ((column (car the-case))
        (row    (cdr the-case)))
    (loop for c from column downto 0
          for r from row to max-row
          collect (cons c r))))

(defun make-right-diagonal (the-case
                            &optional
                              (max-row 5)
                              (max-column 6))
  "Donne les diagonales allant d'en bas à gauche,
jusqu'en bas à droite."
  (let ((column (car the-case))
        (row    (cdr the-case)))
    (loop for c from column to max-column
          for r from row to max-row
          collect (cons c r))))

(defparameter *top-right-corner* '((3 . 0) (4 . 0) (5 . 0)
                                   (6 . 0) (6 . 1) (6 . 2))
  "Coordonnées des points se trouvant en haut à droite.")

(defparameter *top-left-corner* '((3 . 0) (2 . 0) (1 . 0)
                                  (0 . 0) (0 . 1) (0 . 2))
  "Coordonnées des points se trouvant en haut à gauche.")

;; Cette macro permet d'écrire les fonctions
;; vérifiant le nombre de connections diagonales
;; de manière bien plus simple et concise.
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

;;; Définition des fonctions
;;; left-diagonal-connections et
;;; right-diagonal-connections.
(max-in-some-diagonal left-diagonal-connections
                      color board  make-left-diagonal *top-right-corner*)
(max-in-some-diagonal right-diagonal-connections
                      color board make-right-diagonal *top-left-corner*)

(defun connections (color board)
  "Donne le nombre maximal de connections sur le plateau."
  (max
   (vertical-connections       color board)
   (horizontal-connections     color board)
   (left-diagonal-connections  color board)
   (right-diagonal-connections color board)))

(defun winningp (board)
  "Si le nombre maximal de connection sur le plateau est supérieur
à trois, un joueur a gagné."
  (or (> (connections 'yellow board) 3)
      (> (connections 'red    board) 3)))

(defun evaluation (color board)
  (flet ((eval-num (a)
           (case a (0 0) (1 1) (2 4) (3 9) (4 500) (t 500))))
    (let ((vert (vertical-connections       color board))
          (hori (horizontal-connections     color board))
          (l-di (left-diagonal-connections  color board))
          (r-di (right-diagonal-connections color board)))
      (reduce #'+ (mapcar #'eval-num (list vert hori
                                           l-di r-di))))))

(defun play-repl ()
  "L'interface utilisateur, permettant de jouer contre un autre joueur."
  (loop while (and (not (winningp *board*))
                   (not (tiedp *board*)))
        do (progn
             (when (winningp *board*)
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
                 (t  (setf *board* (play i 'yellow *board*))(format t "Eval to: ~D~%" (evaluation 'yellow *board*))))))
        do (progn
             (when (winningp *board*)
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
                 (t (setf *board* (play i 'red *board*))
                    (format t "Eval to: ~D~%"
                            (evaluation 'red *board*))))))))

             ;;;(setf *board* (computer-turn 'red *board*)))))
