(require :cl-csv)

(defstruct city
  name longitude latitude)

(defparameter *belgian-cities*
  (make-array 2787 :initial-contents
            (mapcar
               (lambda (lat)
                 (let ((city-name (second lat))
                       (city-longitude (read-from-string (third lat)))
                       (city-latitude (read-from-string (fourth lat))))
                   (make-city :name city-name
                              :longitude city-longitude
                              :latitude city-latitude)))
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

(defun get-reachable (city)
  (remove-if
   (lambda (other-city)
     (< 20 (get-distance other-city city)))
   *belgian-cities*))

