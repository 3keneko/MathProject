(set-macro-character #\> (get-macro-character #\)))

(set-macro-character
 #\<
 (lambda (stream char)
   (declare (ignore char))
   (let ((contents (read-delimited-list #\> stream t)))
     `(lambda (_)
        ,contents))))
