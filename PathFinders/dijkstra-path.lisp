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
                     #P"belgian-cities-geocoded/belgian-cities-geocoded.csv")))))


(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun get-distance (city1 city2)
  "Nous donne la formule entre deux points à partir de leurs longitudes, via la formule de Haversine, cf.
https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula"
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
  (remove-if
   (lambda (other-city)
     (or (< 20 (get-distance other-city city))
         (equal other-city city)))
     *belgian-cities*))

(defun heuristic-comp (city)
  (- 0 (+ (city-heuristic city)
          (city-cost city))))

(defmacro get-max (heap)
  `(serapeum:heap-extract-maximum ,heap))

(defun make-path (curr path)
  (reverse (cons curr path)))

(defmacro for-it-over (function set &body body)
  `(loop for it across (map 'vector
                            #',function
                            ,set)
         ,@body))

(defun shortest-a*-path (start goal)
  (let ((closed-list nil)
        (priority-queue (serapeum:make-heap
                          :key #'heuristic-comp)))
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
