(defun pgcd (a b)
  (if (zerop b)
      a
      (pgcd b (mod a b))))
