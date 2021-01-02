(require :cl-csv)
(require :serapeum)

(defstruct city
  name longitude latitude cost heuristic prob-cost)

(defparameter *belgian-cities*
  (make-array 2787 :initial-contents
            (mapcar
               (lambda (lat)
                 (let ((city-name
                         (second lat))
                       (city-longitude
                         (read-from-string (third lat)))
                       (city-latitude
                         (read-from-string (fourth lat))))
                   (make-city :name city-name
                              :longitude city-longitude
                              :latitude city-latitude
                              :cost -1
                              :heuristic 0
                              :prob-cost 0)))
               (cdr (cl-csv:read-csv
                     #P"belgian-cities-geocoded/belgian-cities-geocoded.csv"))))
  "Récupère les éléments du CSV et les entrepose dans ce grand tableau.")


(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun get-distance (city1 city2)
  "Nous donne la formule entre deux points à partir de leurs longitudes, via la formule de Haversine, cf.
https://stackoverflow.com/questions/27928/calculate
-distance-between-two-latitude-longitude-points
-haversine-formula"
    (let* ((earth-radius 6371)
           (diff-lat
             (deg->rad (- (city-latitude city1)
                          (city-latitude city2))))
           (diff-lon
             (deg->rad (- (city-longitude city1)
                          (city-longitude city2))))
           (a (+ (* (sin (/ diff-lat 2))
                    (sin (/ diff-lat 2)))
                 (* (cos (deg->rad
                          (city-latitude city1)))
                    (cos (deg->rad
                          (city-latitude city2)))
                    (sin (/ diff-lon 2))
                    (sin (/ diff-lon 2))))))
      (* earth-radius
         (* 2 (atan (sqrt a)
                    (sqrt (- 1 a)))))))

(defun neighbors (city)
  "Remet toutes les villes se trouvant
à proximité de notre ville d'origine."
  (remove-if
   (lambda (other-city)
     (or (< 20 (get-distance other-city city))
         (equal other-city city)))
     *belgian-cities*))

(defun heuristic-comp (city)
  "La fonction heuristique en elle-même,
celle-ci nous permet d'affecter une
priorité aux éléments du tas."
  (- 0 (+ (city-heuristic city)
          (city-cost city))))

(defmacro get-max (heap)
  `(serapeum:heap-extract-maximum ,heap))

(defun make-path (curr path)
  "Reconstruis le chemin à partir des noeuds visités."
  (reverse (cons curr path)))

;; Macro anaphorique, applique la fonction function
;; sur le set, entrepose chacun de ces résultats
;; dans "it".
(defmacro for-it-over (function set &body body)
  `(loop for it across (map 'vector
                            #',function
                            ,set)
         ,@body))

;;;; ALGORITHME A*

(defun shortest-a*-path (start goal)
  "Implémentation de l'algorithme A* en Common Lisp."
  ;; Création de la file de priorité,
  ;; et de la liste des noeuds visités.
  (let ((closed-list nil) 
        (priority-queue (serapeum:make-heap
                         :key #'heuristic-comp)))
    ;; Le départ est bien souvent
    ;; le premier noeud à devoir être visité.
    (serapeum:heap-insert priority-queue start)
    (loop when (serapeum:heap-maximum priority-queue)
            do (let
                ((new-city (get-max priority-queue)))
               (if (equal new-city goal)
                   (return (make-path new-city closed-list)))
                   (for-it-over (lambda (c)
                                  (setf (city-heuristic c)
                                        (get-distance c
                                        goal))
                                  (setf (city-prob-cost c)
                                        (+ (get-distance c
                                            new-city)
                                           (city-cost
                                            new-city)))
                                 c)
                                (neighbors new-city)
                        ;; !! Les "it" s'expliquent
                        ;; par l'utilisation de la
                        ;; macro anaphorique
                        ;; for-it-over.
                        when (and
                              (not (member it closed-list))
                              (or  (eql -1 (city-cost it))
                                   (<  (city-prob-cost it)
                                       (city-cost it))))
                        do (progn
                             (setf (city-cost it)
                                   (city-prob-cost it))
                             (serapeum:heap-insert
                                 priority-queue it))
                 finally (setf closed-list
                               (cons new-city closed-list))))))) 
