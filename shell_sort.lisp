(defun shell-sort (lat gaps)
  (let* ((arr (make-array (length lat) :initial-contents lat)))
    (loop for gap in gaps
          do (loop for i from gap below (length arr)
                   for temp = (aref arr i)
                               do (loop for j from i downto gap by gap
                                     while (> (aref arr (- j gap)) temp)
                                     do (rotatef (aref arr j)
                                              (aref arr (- j gap)))
                                    finally (setf (aref arr j) temp))))
    arr))

(defun fibo-under (x)
  (let ((a 1)
        (b 1)
        (c 0))
    (loop while (< a x)
          collect a into fibo-sequence
          do (setf c (+ a b)
                   b a
                   a c)
          finally (return (reverse fibo-sequence)))))

(defun fibo-shell-sort (lat)
  (shell-sort lat (fibo-under (floor (length lat) 2))))
