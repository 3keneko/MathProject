(defun odds-and-not-odds (list)
  (loop for number in list
        when (oddp number)
          do (princ number)
        do (princ "Odd number found!")))
