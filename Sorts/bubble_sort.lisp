(defun bubble-sort (lat)
  "Implémentation non-récursive, avec copie,
   de l'algorithme du tri à bulles."
  (let ((arr (make-array (length lat) :initial-contents lat)))
    (loop for i from (1- (length arr)) downto 1
            do (loop for j from 0 to (1- i)
                    when (< (aref arr (1+ j)) (aref arr j))
                        do (rotatef (aref arr (1+ j))
                                    (aref arr j))))
    arr))
