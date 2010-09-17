(in-package :planet-wars)

(defun play (&key (input *standard-input*) (output *standard-output*))
  (loop while (peek-char nil input nil nil)
	for turn from 1
	do (let ((game (read-game input)))
	     (write-orders (compute-orders game) output)
	     (write-line "go" output))))
   
(defun random-elt (sequence)
  (if (> (length sequence) 0)
      (elt sequence (random (length sequence)))
      nil))

(defun enemy-planet-p (planet)
  (> (owner planet) 1))

(defun own-planet-p (planet)
  (= (owner planet) 1))

(defun neutral-planet-p (planet)
  (= (owner planet) 0))

(defun enemy-planets (game)
  (remove-if-not #'enemy-planet-p (planets game)))

(defun own-planets (game)
  (remove-if-not #'own-planet-p (planets game)))

(defun neutral-planets (game)
  (remove-if-not #'neutral-planet-p (planets game)))

(defun other-planets (game)
  (remove-if #'own-planet-p (planets game)))
