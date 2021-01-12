(defun merge-sort (arr)
  "Implémentation du tri par fusion,
en utilisant la fonction merge, prédéfinie."
  (if (<= (length arr) 1)
      arr
      (let* ((middle (floor (length arr) 2))
             (left (subseq arr 0 middle))
             (right (subseq arr middle (length arr))))
          (merge 'list (merge-sort left)
                 (merge-sort right) #'<))))
