(defun quick-sort (lat)
  "Implémentation récursive de l'algorithme de tri-rapide."
  (if lat
    (let ((partition (car lat))
          (rest (cdr lat)))
      (append
       (quick-sort (remove-if (lambda (a)
                                (>= a partition))
                              rest))
       (cons partition
             (quick-sort (remove-if (lambda (a)
                          (< a partition))
                        rest)))))
    nil))

