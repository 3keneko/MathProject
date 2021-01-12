(ql:quickload :serapeum)

(defparameter *latin->morse*
  (serapeum:dict #\a "._" #\b "_..." #\c "_._." #\d "_.."
                 #\e "." #\f ".._." #\g "__." #\h "...."
                 #\i ".." #\j ".___" #\k "_._" #\l "._.."
                 #\m "__" #\n "_." #\o "___" #\p ".__."
                 #\q "__._" #\r "._." #\s "..." #\t "_"
                 #\u ".._" #\v "..._" #\w ".__" #\x "_.._"
                 #\y "_.__" #\z "__.." #\1 ".____" #\2 "..___"
                 #\3 "...__" #\4 "...._" #\5 "....." #\6 "_...."
                 #\7 "__..." #\8 "___.." #\9 "____." #\0 "_____"
                 #\SPACE " "))

(defun convert-to-morse (sentence)
  "Cette fonction s'occupe de convertir
nos phrases vers du code Morse, grâce au dictionnaire
créé ci-dessus!"
  (let ((converted-list
          (loop for letter across sentence 
                collect (gethash letter *latin->morse*))))
  (format nil "~{~A~}" converted-list)))
