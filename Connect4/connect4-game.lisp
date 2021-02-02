(defconstant +inf most-positive-fixnum)
(defconstant -inf (* -1 +inf))

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

(defun get-legal (board)
  (loop for i from 0 to 6
        if (not (fullp i board))
          collect i))

(defun tiedp (board)
  "Vérifie si la position est nulle."
  (notany #'null
          (map 'list #'car board)))

(defun play (i color board)
  "Fonction permettant de jouer un jeton sur le plateau."
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

(defstruct move column-choice eval-state)

(defun max-move (mov1 mov2)
  (if (> (move-eval-state mov1) (move-eval-state mov2)) mov1 mov2))

(declaim (inline max-to-color))

(defun max-to-color (n)
  (if (eql n 1)
      'red
      'yellow))

(defun min-move (mov1 mov2)
  (if (< (move-eval-state mov1) (move-eval-state mov2)) mov1 mov2))

(defmacro defmemo (fun-name args &body body)
  (let ((big-hash (gensym)))
    `(let ((,big-hash (make-hash-table :test #'equalp)))
  (defun ,fun-name ,args
    (if (gethash ,(car args) ,big-hash)
        (gethash ,(car args) ,big-hash)
        (setf (gethash ,(car args) ,big-hash) ,@body))))))

;; STATE PASSING-U.
(defmemo minimax (board depth maximizing
                &optional (last-move -1) (alpha -inf) (beta  +inf))
  (cond
    ((or (tiedp board) (eql depth 0) (winningp board))
        (make-move :column-choice last-move
                   :eval-state (evaluation board)))
    (t (let ((best-score (* -inf maximizing))
             (best-move -1))
     (loop for choice from 0 to 6
           for new-state = (play choice (max-to-color maximizing)
                                (copy-seq board))
           for state = (minimax new-state (1- depth)
                                (* -1 maximizing) choice alpha beta)
           when (or (and (> (move-eval-state state) best-score)
                         (eql maximizing 1))
                    (and (< (move-eval-state state) best-score)
                         (eql maximizing -1)))
             do (progn
                    (setf best-score (move-eval-state state))
                    (setf best-move  (move-column-choice state))
                    (cond ((>= best-score alpha) (setf alpha best-score))
                          ((<= best-score beta)  (setf beta  best-score))))
          when (>= alpha beta) do (return))
    (make-move :column-choice best-move
               :eval-state best-score)))))

#|
(defun minimax (board depth color
                        &optional (last-move -1)
                        (alpha most-negative-fixnum) (beta most-positive-fixnum))
  (let ((c-board (copy-seq board))
        (curr (evaluation color board)))
    (cond
      ((eql depth 0) 
       (progn
         (format t "~A~%" (make-move :column-choice last-move
                                     :eval-state curr))
         (make-move :column-choice last-move
                    :eval-state curr)))
      ((eql color 'red)
       (let ((best-move (make-move :column-choice 0
                                   :eval-state most-negative-fixnum)))
         (loop for i in (get-legal *board*)
               for new-board = (play i 'red c-board)
               do (progn
                    ;;(show-board new-board)
                    ;;(format t "Evaluation: ~D~%" (evaluation 'red new-board))
                    (setf best-move
                        (max-move best-move
                                  (minimax new-board (1- depth) 'yellow i alpha beta)))
                    (setf alpha (max alpha (move-eval-state best-move)))
                    (when (>= alpha beta)
                        (return))))
         best-move))
      (t
       (let ((best-move (make-move :column-choice 0
                                   :eval-state most-positive-fixnum)))
          (loop for i in (get-legal *board*)
                for new-board = (play i 'yellow c-board)
                do (progn
                    ;;(show-board new-board)
                    ;;(format t "Evaluation: ~D~%" (evaluation 'red new-board))
                     (setf best-move
                           (min-move best-move
                                     (minimax new-board (1- depth) 'red i alpha beta)))
                     (setf beta (min beta (move-eval-state best-move)))
                     (if (<= beta alpha)
                         (return))))
         best-move)))))
|#

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
             (t (setf ,board (play i ',color ,board))))))))

(defun play-against-player ()
  "L'interface utilisateur, permettant de jouer contre un autre joueur."
  (loop while (not (or (winningp *board*)
                       (tiedp *board*)))
        do (player-repl *board* 1 play-against-player)
        do (player-repl *board* 2 play-against-player)))

(defun play-against-computer ()
  (loop while (not (or (winningp *board*)
                       (tiedp *board*)))
        do (setf *board* (play (move-column-choice (minimax *board* 1 1)) 'red *board*))
        do (player-repl *board* 1 play-against-computer)
        if (winningp *board*)
          do (return "Vous avez gagné!")))
