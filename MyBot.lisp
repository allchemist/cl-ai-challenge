(load "split-sequence")
(load "planet-wars")

(in-package :pwbot)

;; utils

(defun info (msg &optional prefix)
  (if prefix
      (format *query-io* "~A: ~A~%" prefix msg)
      (format *query-io* "~A~%" msg)))

(defun square (x) (* x x))

(defun planet-distance (p1 p2)
  (+ (square (- (x p1) (x p2)))
     (square (- (y p1) (y p2)))))

(defun sort-by-distance (planet other-planets)
  (sort (mapcar #'copy-planet other-planets) '<
	:key #'(lambda (p) (planet-distance p planet))))

(defun sort-by-growth (planets)
  (sort (mapcar #'copy-planet planets) '>
	:key #'growth))

(defun sort-by-nships (planets)
  (sort (mapcar #'copy-planet planets) '>
	:key #'n-ships))

(defun normalize (list)
  (let ((norm (sqrt (reduce #'+ (mapcar #'square list)))))
    (mapcar #'(lambda (x) (/ x norm)) list)))

(defun scal (list const)
  (mapcar #'(lambda (l) (* l const)) list))

;; here the bot code should be

(load "bot1")

;; here the bot code ends

;; main

(defun play (&key (input *standard-input*) (output *standard-output*))
  (loop while (peek-char nil input nil nil)
	for turn from 1
	do (let ((game (read-game input)))
	     (write-orders (compute-orders game) output)
	     (write-line "go" output))))

(defun main ()
  (let ((*random-state* (make-random-state t)))
    (play)))
