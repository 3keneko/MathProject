(defparameter *heaps* nil)

(defun unsafep (heaps)
  (= 0 (reduce #'logxor heaps)))

(defun emptiedp (heaps)
  (= 0 (reduce #'+ heaps)))

(defun play-random-legal-move (heaps)
  (let ((unchanged t))
    (loop for heap in heaps
          for heap-number from 1
          while unchanged
          when (> 0 heap)
            do (let ((number-of-matches-c (1+ (random heap))))
                (format t "L'ordinateur a décidé de prendre ~D alumettes du tas n°~D~%"
                        number-of-matches-c heap-number)
                (decf (nth (1- heap-number) heaps) number-of-matches-c)
                (setf unchanged nil))))
    heaps)

(defun winningp (num index heaps)
  (let ((safe-copy (copy-list heaps)))
    (decf (nth index safe-copy) num)
    (unsafep safe-copy)))

(defun play-winning-move (heaps)
  (let ((unchanged t))
    (loop for heap in heaps
          for i from 0
          while unchanged
          do (loop for num from 0 to heap
                  when (winningp num i heaps)
                    do (progn
                          (format t "L'ordinateur a décidé de prendre ~D alumettes du tas ~D~%"
                                  num (1+ i))
                          (decf (nth i heaps) num)
                          (setf unchanged nil)))))
  heaps)

(defun c-move (heaps)
  (cond
    ((emptiedp heaps)
     (format t "Félicitations, vous avez gagné!~%"))
    ((unsafep heaps)
     (play-random-legal-move heaps))
    (t (play-winning-move heaps))))

(defun heap-print (heaps)
  (format t "~&~{TAS N°~D: ~D alumettes.~%~}"
          (loop for number from 1 to (length *heaps*)
                for heap in heaps
                collect number
                collect heap)))

(defun heaps-repl ()
  (when (emptiedp *heaps*)
    (format t "Pas de chance, l'ordinateur a gagné.~%A une prochaine fois peut-être."))
  (format t "C'est votre tour, de quel tas désirez vous retirer des alumettes? ")
  (heap-print *heaps*)
  (princ 'PLAYER> )
  (let ((player-heap-choice (read)))
    (if (or (> player-heap-choice (length *heaps*))
            (>= 0 player-heap-choice))
      (progn (format t "Veuillez choisir un tas compris entre 1 et ~D~%" (length *heaps*))
             (heaps-repl))
      (progn
        (format t "Combien d'alumettes désirez-vous retirer de ce tas?~%")
        (princ 'PLAYER>)
        (let ((player-heap-choice (1- player-heap-choice))
              (number-of-matches (read)))
            (if (or (< (nth player-heap-choice *heaps*) number-of-matches)
                    (>= 0 number-of-matches))
                (progn (format t "Veuillez choisir un nombre correct d'éléments.~%")
                    (heaps-repl))
                (decf (nth player-heap-choice *heaps*) number-of-matches)))))))


(defun nim-game-repl ()
  (unless *heaps*
    (format t "Avec quels tas voulez-vous jouer? ")
    (setf *heaps* (read-from-string
                   (format nil "(~A)" (read-line))))
    (format t "Très bien, que le jeu commence!~%"))
  (heaps-repl)
  (setf *heaps* (c-move *heaps*))
  (unless (emptiedp *heaps*)
    (nim-game-repl)))
