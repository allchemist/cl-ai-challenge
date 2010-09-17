(defpackage :planet-wars
    (:use :cl :asdf))

(in-package :planet-wars)

(defsystem planet-wars
  :name "Planet Wars Bot for the Google AI contest"
  :author "Khokhlov Ivan"
  :licence "MIT"
  :depends-on (split-sequence)
  :components
  ((:file "model")
   (:file "io")
   (:file "play"))
  :serial t)
