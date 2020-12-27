(defun bubble-sort (lat)
  "Implémentation non-récursive, avec copie,
   de l'algorithme du tri à bulles."
  (let ((lat-copy (make-array (length lat) :initial-contents lat)))
    (loop for i from (1- (length lat-copy)) downto 1
            do (loop for j from 0 to (1- i)
                    when (< (aref lat-copy (1+ j)) (aref lat-copy j))
                        do (rotatef (aref lat-copy (1+ j))
                                    (aref lat-copy j))))
    lat-copy))
