(defconstant +inf most-positive-fixnum)
(defconstant -inf (* -1 +inf))

(defparameter *board*
  (make-array 7 :initial-element
              (make-list 6 :initial-element nil))
  "Le plateau de puissance 4 en lui-même.")

(defparameter *moves* nil
  "Reprend la liste des coups ayant été jouée.")

(defun make-new-board ()
  "Permet de créer un plateau de jeu tout neuf."
  (setf *board* (make-array 7 :initial-element
                            (make-list 6 :initial-element nil)))
  (setf *moves* nil))

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

(defun get-legal (board)
  (loop for i from 0 to 6
        if (not (fullp i board))
          collect i))

(defun tiedp (board)
  "Vérifie si la position est nulle."
  (notany #'null
          (map 'list #'car board)))

(defun play (i color board &optional (mark nil))
  "Fonction permettant de jouer un jeton sur le plateau."
  (when mark
    (setf *moves* (append *moves* (list i))))
  (let ((c-board (copy-seq board)))
    (labels ((push-until (lat token)
                (cond
                ((eq 1 (length lat)) `(,token))
                ((cadr lat) (cons token (cdr lat)))
                (t (cons (car lat)
                        (push-until (cdr lat) token))))))
        (setf (aref c-board i) (push-until (aref c-board i) color))
    c-board)))


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

(defun evaluation (board)
  "Fonction permettant d'attribuer une valeur à chaque position."
  (cond 
      ((tiedp board) 0)
      (t (flet ((eval-num (a) (case a (0 0) (1 4) (2 550) (3 3500) (t 50000000))))
            (let ((vert (vertical-connections       'red board))
                  (hori (horizontal-connections     'red board))
                  (l-di (left-diagonal-connections  'red board))
                  (r-di (right-diagonal-connections 'red board))
                  (vero (vertical-connections       'yellow board))
                  (horo (horizontal-connections     'yellow board))
                  (l-do (left-diagonal-connections  'yellow board))
                  (r-do (right-diagonal-connections 'yellow board)))
            (- (reduce #'+ (mapcar #'eval-num (list vert hori l-di r-di)))
               (reduce #'+ (mapcar #'eval-num (list vero horo l-do r-do)))))))))

(declaim (inline max-to-color))
(defun max-to-color (n)
  (if (eql n 1)
      'red
      'yellow))

(defun get-max-min-seq (lst key)
  "Donne la séquence de coups ayant la meilleure évaluation, selon la couleur."
  (let ((max-seq
          (reduce (lambda (a b)
            (if (funcall key (cdr a) (cdr b))
                a
                b))
            lst)))
          (nbutlast (car max-seq))
          max-seq))

(defun group-by (list key)
  "Groupe les éléments d'une liste selon un certain prédicat/fonction."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type cons list) (type function key))
  (labels ((grouper (lat the-key acc)
           (if (null lat)
               acc
               (let ((test-val (funcall the-key (car lat))))
                 (grouper (remove-if (lambda (a)
                                       (equal (funcall the-key a) test-val))
                                lat)
                          the-key
                          (cons (remove-if-not (lambda (a)
                                                 (equal (funcall the-key a) test-val))
                                               lat)
                                acc))))))
    (grouper list key nil)))

(defun max-len-filtr (big-list &key (fn 'identity))
  "Retourne toutes les listes de tailles maximales,
ainsi que la longueur de ces listes, et une liste contenant les listes de tailles inférieures."
  (let ((max-len    0)
        (acc      nil)
        (rejected nil))
    (loop for list in big-list
          do (cond ((> (length (funcall fn list)) max-len)
                    (setf max-len (length (funcall fn list)))
                    (loop for list in acc
                          do (push list rejected))
                    (setf acc (make-list 1 :initial-element list)))
                   ((= (length (funcall fn list)) max-len)
                    (push list acc))
                   (t (push list rejected))))
    (values acc max-len rejected)))

(defun make-moves (moves)
  "Permet de trouver une position uniquement en fonction des coups joués."
  (let ((first-player 'red)
        (board (make-array 7 :initial-element
                           (make-list 6 :initial-element nil))))
    (flet ((get-opp (color)
             (case color
               (red 'yellow)
               (t   'red))))
      (loop for move in moves
            do (setf board (play move first-player board))
               (setf first-player (get-opp first-player)))
      board)))

(defun eval-sequence (moves)
  "Permet d'évaluer une position uniquement en fonction des coups joués."
  (evaluation (make-moves moves)))

(defun parity (num)
  "Fonction retournant -1 si le nombre est pair, 1 sinon."
  (if (evenp num) -1 1))

(defparameter *all-possible-moves* nil
  "Variable globale reprennant tout les coups possibles retournés par notre algorithme minimax.")

(defun minmax (move-seq depth)
  "Retourne toutes les suites de coups possibles, ainsi que l'évaluation leur étant attribuée."
  (let ((board (make-moves move-seq)))
    (cond
      ((tiedp board)
       (push (cons move-seq 0) *all-possible-moves*))
      ((winningp board)
       (push (cons move-seq (* +inf (parity (length move-seq))))
             *all-possible-moves*))
      ((= depth 0)
       (push (cons move-seq (eval-sequence move-seq))
             *all-possible-moves*))
      (t (loop for move in (get-legal board)
               do (minmax (append move-seq (list move)) (1- depth))))))
  *all-possible-moves*)

(defun flatten (nested)
  "Permet d'aplatir une liste."
  (reduce #'nconc nested))

(defun best-play (depth moves)
  "Retourne le meilleur coup possible en fonction de la profondeur."
  (minmax moves depth)
  (mapc (lambda (lst)
          (setf (car lst)
                (nthcdr (length moves)
            (car lst))))
    *all-possible-moves*)
  (loop (multiple-value-bind (valuable max-len rejected)
            (max-len-filtr *all-possible-moves* :fn #'car)
          (when (= max-len 0)
            (format t "An error Occured, caught!") (return))
          (when (= max-len 1) (return))
          (setf *all-possible-moves*
             (mapcar (lambda (a)
                       (get-max-min-seq a
                           (lambda (c d) (if (evenp max-len) (< c d) (> c d)))))
                    (group-by valuable (lambda (a) (butlast (car a))))))
          (setf *all-possible-moves* (append rejected *all-possible-moves*))
       (caar (reduce (lambda (a b)
                  (if (> (cdr a) (cdr b)) a b))
                *all-possible-moves*)))

(defmacro player-repl (board player-turn context-name)
  (let ((other-player (if (eql player-turn 2) 1 2))
        (color (if (eql player-turn 1) 'yellow 'red)))
    `(progn
        (when (winningp ,board)
          (return (format nil "Le joueur ~D a gagné." ,other-player)))
        (format t "Au tour du joueur ~D!~%~A~%Dans quelle colonne souhaitez vous jouer? " 
                ,player-turn (show-board ,board))
        (let* ((column (read))
               (i (1- column)))
          (cond
            ((not (<= 1 column 7))
                  (format t "Veuillez jouer un numéro de colonne correct!~%")
                  (,context-name))
             ((fullp i ,board)
              (format t "Cette colonne est déjà remplie!~%")
              (,context-name))
             (t (setf ,board (play i ',color ,board t))))))))

(defun play-against-player ()
  "L'interface utilisateur, permettant de jouer contre un autre joueur."
  (loop while (not (or (winningp *board*)
                       (tiedp *board*)))
        do (player-repl *board* 1 play-against-player)
        do (player-repl *board* 2 play-against-player)))

(defun play-against-computer ()
  (loop while (not (or (winningp *board*)
                       (tiedp *board*)))
        do (setf *board* (play (best-play 4 *moves*) 'red *board* t))
           (setf *all-possible-moves* nil)
           (player-repl *board* 1 play-against-computer)
        when (winningp *board*)
          do (return "Vous avez gagné!")))
