(in-package :planet-wars)

;; This code was taken from Gabor Melis' starter-pack
;; with slight modifications

;; planet level

(defun parse-planet (line)
  (let ((tokens (split-sequence:split-sequence #\space line)))
    (assert (string= "P" (elt tokens 0)))
    (make-planet :x (read-from-string (elt tokens 1))
                 :y (read-from-string (elt tokens 2))
                 :owner (read-from-string (elt tokens 3))
                 :n-ships (read-from-string (elt tokens 4))
                 :growth (read-from-string (elt tokens 5)))))

(defun read-planet (stream)
  (parse-planet (read-line stream)))

(defun write-planet (planet stream)
  (format stream "P ~F ~F ~D ~D ~D~%" (x planet) (y planet)
          (owner planet) (n-ships planet) (growth planet)))

;; fleet level

(defun parse-fleet (line)
  (let ((tokens (split-sequence:split-sequence #\space line)))
    (assert (string= "F" (elt tokens 0)))
    (make-fleet :owner (read-from-string (elt tokens 1))
                :n-ships (read-from-string (elt tokens 2))
                :source (read-from-string (elt tokens 3))
                :destination (read-from-string (elt tokens 4))
                :n-total-turns (read-from-string (elt tokens 5))
                :n-remaining-turns (read-from-string (elt tokens 6)))))

(defun read-fleet (stream)
  (parse-fleet (read-line stream)))

(defun write-fleet (fleet stream)
  (format stream "F ~D ~D ~D ~D ~D ~D~%" (owner fleet) (n-ships fleet)
          (source fleet) (destination fleet) (n-total-turns fleet)
          (n-remaining-turns fleet)))

;; game level

(defun read-game (stream)
  (let ((planets ())
        (fleets ())
        (next-planet-id 0))
    (loop for line = (read-line stream nil nil)
          while (and line (not (string= "go" line)))
          do
          (when (plusp (length line))
            (cond ((char= #\P (aref line 0))
                   (let ((planet (parse-planet line)))
                     (setf (id planet) next-planet-id)
                     (incf next-planet-id)
                     (push planet planets)))
                  ((char= #\F (aref line 0))
                   (push (parse-fleet line) fleets)))))
    (make-game :planets (nreverse planets)
               :fleets (nreverse fleets))))

(defun write-game (game stream)
  (map nil (lambda (planet)
             (write-planet planet stream))
       (planets game))
  (map nil (lambda (fleet)
             (write-fleet fleet stream))
       (fleets game)))

;; order level

(defun write-order (order stream)
  (format stream "~D ~D ~D~%" (planet-id (order-source order))
          (planet-id (order-destination order))
          (order-n-ships order)))

(defun write-orders (orders stream)
  (map nil (lambda (order)
             (write-order order stream))
       orders))
