(require :asdf)
(require :planet-wars)

(in-package :planet-wars)

;; random order

(defun compute-orders (game)
  (let ((source (random-elt (own-planets game))))
    (list (make-order :source source
		      :destination (random-elt (append (neutral-planets game) (enemy-planets game)))
		      :n-ships (random (n-ships source))))))

;; make an executable bot

(sb-ext:save-lisp-and-die "MyBot"
  :toplevel #'play
  :save-runtime-options t
  :executable t)
