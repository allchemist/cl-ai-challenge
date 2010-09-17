(in-package :planet-wars)

;; This code was taken from Gabor Melis' starter-pack

(defstruct (%ships (:conc-name ""))
  owner
  n-ships)

(defstruct (planet (:include %ships) (:conc-name ""))
  id
  x
  y
  growth)

(defstruct (fleet (:include %ships) (:conc-name ""))
  source
  destination
  n-total-turns
  n-remaining-turns)

(defstruct (game (:conc-name ""))
  planets
  fleets)

(defstruct order
  source
  destination
  n-ships)

(defun planet-id (obj)
  (if (planet-p obj)
      (id obj)
      obj))
