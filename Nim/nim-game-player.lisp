(defparameter *heaps* nil
  "*heaps* représente les différents tas avec lesquels nous allons jouer.")

(defun unsafep (heaps)
  "Vérifie si la nim-somme des tas est égale à 0."
  (= 0 (reduce #'logxor heaps)))

(defun emptiedp (heaps)
  "Vérifie si les tas sont vides."
  (= 0 (reduce #'+ heaps)))

(defun play-random-legal-move (heaps)
  "Si l'adversaire est en position gagnante, nous prendrons un nombre au hasard
d'allumettes sur le premier tas à disposition."
  (let ((unchanged t))
    (loop for heap in heaps
          for heap-number from 1
          while unchanged
          when (< 0 heap)
            do (let ((number-of-matches-c
                       (1+ (random heap))))
                (format t "L'ordinateur a décidé de prendre ~D allumettes du tas ~D~%"
                        number-of-matches-c heap-number)
                (decf (nth (1- heap-number) heaps) number-of-matches-c)
                (setf unchanged nil)))
    heaps))

(defun winningp (num index heaps)
  "Vérifie si une position serait hypothétiquement gagnante."
  (let ((safe-copy (copy-list heaps)))
    (decf (nth index safe-copy) num)
    (unsafep safe-copy)))

(defun play-winning-move (heaps)
  "Si l'adversaire n'est pas en poistion gagnante, nous jouons alors le meilleur
coup disponible."
  (let ((unchanged t))
    (loop for heap in heaps
          for i from 0
          while unchanged
          do (loop for num from 0 to heap
                  when (winningp num i heaps)
                    do (progn
                          (format t "L'ordinateur a décidé de prendre ~D allumettes du tas ~D~%"
                                  num (1+ i))
                          (decf (nth i heaps) num)
                          (setf unchanged nil)))))
  heaps)

(defun c-move (heaps)
  "Cette fonction fait office d'aiguilleur,
| Les tas sont vides -> On arrête de jouer.
| Nous sommes dans une poisition perdante -> On joue un coup au hasard.
| Nous gagnons -> On joue le meilleur coup disponible."
  (cond
    ((emptiedp heaps)
     (format t "Félicitations, vous avez gagné!~%"))
    ((unsafep heaps)
     (play-random-legal-move heaps))
    (t (play-winning-move heaps))))

(defun heap-print (heaps)
  (format t "~&~{TAS ~D: ~D allumettes.~%~}"
          (loop for number from 1 to (length *heaps*)
                for heap in heaps
                collect number
                collect heap)))

(defun heaps-repl ()
  "Cette fonction récursive est la fonction que le joueur voit lorsqu'il joue."
  (when (emptiedp *heaps*)
    (format t "Pas de chance, l'ordinateur a gagné.~%A une prochaine fois peut-être."))
  (format t "C'est votre tour, de quel tas désirez vous retirer des allumettes? ")
  (heap-print *heaps*)
  (princ 'PLAYER> )
  (let ((player-heap-choice (read)))
    (if (or (> player-heap-choice (length *heaps*))
            (>= 0 player-heap-choice))
        (progn (format t "Veuillez choisir un tas compris entre 1 et ~D~%" (length *heaps*))
               ;; Remarquez l'appel récursif à heaps-repl
               ;; si le joueur tente une mauvaise entrée.
             (heaps-repl))
      (progn
        (format t "Combien d'allumettes désirez-vous retirer de ce tas?~%")
        (princ 'PLAYER>)
        (let ((player-heap-choice (1- player-heap-choice))
              (number-of-matches (read)))
            (if (or (< (nth player-heap-choice *heaps*) number-of-matches)
                    (>= 0 number-of-matches))
                (progn (format t "Veuillez choisir un nombre correct d'éléments.~%")
                       ;; Encore un appel récursif à heaps-repl!
                       (heaps-repl))
                ;; Si nous parvenons à passer à travers toutes les conditions,
                ;; alors, nous modifions la valeur des tas.
                (decf (nth player-heap-choice *heaps*) number-of-matches)))))))


(defun nim-game-repl ()
  "La fonction de base pour commencer une partie."
  (unless *heaps*
    (format t "Avec quels tas voulez-vous jouer? ")
    (setf *heaps* (read-from-string
                   (format nil "(~A)" (read-line))))
    (format t "Très bien, que le jeu commence!~%"))
  (heaps-repl) ;; <- Appel de heaps-repl (tour du joueur).
  (setf *heaps* (c-move *heaps*)) ;; <- Appel de c-move (tour de l'ordinateur).
  (unless (emptiedp *heaps*)
    (nim-game-repl)))
