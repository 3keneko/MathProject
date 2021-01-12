(defun quick-sort (lat)
  "Implémentation récursive de l'algorithme de tri-rapide."
  (if lat
      (let ((partition (car lat))
            (rest (cdr lat)))
      (nconc
       (quick-sort (remove-if (lambda (a)
                                (>= a partition))
                              rest))
       (remove-if (lambda (a) (/= a partition)) lat)
       (quick-sort (remove-if (lambda (a)
                                (<= a partition))
                              rest))))
    nil))

