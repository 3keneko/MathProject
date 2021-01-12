(defun sll-bubble-sort (lat)
  "Implémentation non-récursive, avec copie,
   de l'algorithme du tri à bulles,
  sur des listes simplement chaînées"
  (let ((lat-copy (copy-list lat)))
    (loop for i from (1- (length lat-copy)) downto 1
            do (loop for j from 0 to (1- i)
                    when (< (nth (1+ j) lat-copy) (nth j lat-copy))
                        do (rotatef (nth (1+ j) lat-copy)
                                    (nth j lat-copy))))
    lat-copy))
