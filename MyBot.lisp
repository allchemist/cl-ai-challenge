(require :asdf)
(load "planet-wars/planet-wars.asd")
(load "split-sequence/split-sequence.asd")
(require :split-sequence)
(require :planet-wars)

(in-package :planet-wars)

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

;; aiming

(defparameter *dist-coeff* 1.0)
(defparameter *growth-coeff* 1.0)
(defparameter *easy-coeff* 1.0)
(defparameter *choose-of* 10)

(defun define-aim (planet game)
  (let* ((neutrals (neutral-planets game))
	 (enemies (enemy-planets game))
	 (planets (append neutrals enemies))
	 (dist-rating
	  (scal (normalize (mapcar #'(lambda (p) (/ (planet-distance planet p))) planets))
		*dist-coeff*))
	 (growth-rating
	  (scal (normalize (mapcar #'(lambda (p) (growth p)) planets))
		*growth-coeff*))
	 (easy-rating
	  (scal (normalize (mapcar #'(lambda (p) (/ (1+ (n-ships p)))) planets))
		*easy-coeff*))
	 (result-rating (mapcar #'(lambda (d g e) (+ d g e)) dist-rating growth-rating easy-rating))
	 (sorted-rating (stable-sort (copy-list result-rating) #'>)))
    (let ((dest (elt planets (position (first sorted-rating) result-rating))))
      (cond ((member dest neutrals :test #'equalp)
	     (when (> (n-ships planet) (+ (n-ships dest) 2))
	       (list dest (1+ (n-ships dest)))))
	    ((member dest enemies :test #'equalp)
	     (when (> (n-ships planet) (* 10 (growth planet)))
	       (list dest (floor (/ (n-ships planet) 3)))))))))

;; orders

(defun compute-orders (game)
  (let* ((source (random-elt (own-planets game)))	 
	 (others (append (neutral-planets game) (enemy-planets game)))
	 (dest (when (> (length others) 0) (define-aim source game))))
    (when (and source dest)
      (list
       (make-order :source source
		   :destination (first dest)
		   :n-ships (second dest))))))

#|
(defun compute-orders (game)
  (when (and (own-planets game) (enemy-planets game))
    (remove nil (mapcar #'(lambda (p)
			    (let ((aim (define-aim p game)))
			      (make-order :source p
					  :destination (first aim)
					  :n-ships (1- (second aim)))))
			(own-planets game)))))
|#

#|
(sb-ext:save-lisp-and-die "MyBot"
  :toplevel #'play
  :save-runtime-options t
  :executable t)
|#

(defpackage :pwbot
    (:use #:planet-wars #:common-lisp)
  (:export #:main))

(in-package :pwbot)

(defun main ()
  (let ((*random-state* (make-random-state t)))
    (planet-wars::play)))
