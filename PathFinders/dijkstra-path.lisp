(require :cl-csv)
(require :serapeum)

(defstruct city
  name longitude latitude cost heuristic prob-cost)

(defmacro make-random-path (x arr size)
  `(loop for _ below ,x
         collect (aref ,arr (random ,size))))

(defparameter *belgian-cities*
  (make-array 2787 :initial-contents
            (mapcar
               (lambda (lat)
                 (let ((city-name (second lat))
                       (city-longitude (read-from-string (third lat)))
                       (city-latitude (read-from-string (fourth lat))))
                   (make-city :name city-name
                              :longitude city-longitude
                              :latitude city-latitude
                              :cost -1
                              :heuristic 0
                              :prob-cost 0)))
               (cdr (cl-csv:read-csv #P"belgian-cities-geocoded/belgian-cities-geocoded.csv")))))


(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun get-distance (city1 city2)
  "Nous donne la formule entre deux points à partir de leurs longitudes, via la formule de Haversine, cf.
https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula"
    (let* ((earth-radius 6371)
           (diff-lat (deg->rad (- (city-latitude city1) (city-latitude city2))))
           (diff-lon (deg->rad (- (city-longitude city1) (city-longitude city2))))
           (a (+ (* (sin (/ diff-lat 2)) (sin (/ diff-lat 2)))
                 (* (cos (deg->rad (city-latitude city1)))
                    (cos (deg->rad (city-latitude city2)))
                    (sin (/ diff-lon 2)) (sin (/ diff-lon 2))))))
      (* earth-radius
         (* 2 (atan (sqrt a) (sqrt (- 1 a)))))))

(defun neighbors (city)
  (remove-if
   (lambda (other-city)
     (or (< 20 (get-distance other-city city))
         (equal other-city city)))
     *belgian-cities*))

;; (defun in-reach (city1 city2)
;;   (> 20 (get-distance city1 city2)))

;;(defun deconstruct-path (the-list)
;;  ...)

(defun shortest-a*-path (start goal)
  (let ((closed-list nil)
        (priority-queue (serapeum:make-heap
                          :key (lambda (c)
                                 (- 0 (+ (city-heuristic c)
                                         (city-cost c)))))))
    (serapeum:heap-insert priority-queue start)
    (loop when (serapeum:heap-maximum priority-queue)
          do (let ((new-city (serapeum:heap-extract-maximum priority-queue)))
               (if (equal new-city goal)
                   (return (reverse (cons new-city closed-list)))
                   (loop for neighbor across (map 'vector (lambda (c)
                                                   (setf (city-heuristic c)
                                                         (get-distance c goal))
                                                   (setf (city-prob-cost c)
                                                         (+ (get-distance c new-city)
                                                            (city-cost new-city)))
                                                   c)
                                                  (neighbors new-city))
                         when (and (not (member neighbor closed-list))
                                   (or  (eql -1 (city-cost neighbor))
                                        (<  (city-prob-cost neighbor)
                                            (city-cost neighbor))))
                           do (progn
                                (setf (city-cost neighbor) (city-prob-cost neighbor))
                                (serapeum:heap-insert priority-queue neighbor))
                         finally (setf closed-list (cons new-city closed-list)))))))) 
